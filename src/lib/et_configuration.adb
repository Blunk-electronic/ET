------------------------------------------------------------------------------
--                                                                          --
--                        SYSTEM ET CONFIGURATION                           --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2018 Mario Blunk, Blunk electronic                 --
--                                                                          --
--    This program is free software: you can redistribute it and/or modify  --
--    it under the terms of the GNU General Public License as published by  --
--    the Free Software Foundation, either version 3 of the License, or     --
--    (at your option) any later version.                                   --
--                                                                          --
--    This program is distributed in the hope that it will be useful,       --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of        --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         --
--    GNU General Public License for more details.                          --
--                                                                          --
--    You should have received a copy of the GNU General Public License     --
--    along with this program.  If not, see <http://www.gnu.org/licenses/>. --
------------------------------------------------------------------------------

--   For correct displaying set tab with in your edtior to 4.

--   The two letters "CS" indicate a "construction side" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with ada.text_io;				use ada.text_io;
with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.strings; 				use ada.strings;
--with ada.strings.maps;			use ada.strings.maps;

-- with ada.characters.handling;	use ada.characters.handling;
-- with ada.strings.fixed; 		use ada.strings.fixed;


with ada.containers;            use ada.containers;
with ada.containers.doubly_linked_lists;


with ada.directories;

with et_general;
with et_coordinates;
with et_libraries;
with et_schematic;
with et_string_processing;		use et_string_processing;
with et_export;
with et_import;

package body et_configuration is

	function to_string (net_comparator_on_off : in type_net_comparator_on_off) return string is
	-- Returns the given net comparator status as string.
	begin
		return "net name comparator " & type_net_comparator_on_off'image (net_comparator_on_off);
	end to_string;

	function to_string (net_comparator_warn : in type_net_comparator_warn_only) return string is
	-- Returns the given net comparator warning status as string.
	begin
		return "warnings only " & type_net_comparator_warn_only'image (net_comparator_warn);
	end to_string;
	
	function to_submodule (abbrevation : in et_coordinates.type_submodule_abbrevation.bounded_string) 
		return type_import_module is
	-- Looks up the container import_modules for the given abbrevation and returns the submodule.
	-- Raises alarm if no submodule could be found -> abbrevation invalid.	
		use et_coordinates.type_submodule_abbrevation;
		use type_import_modules;
		module_cursor : type_import_modules.cursor := import_modules.first;
	begin
		while module_cursor /= type_import_modules.no_element loop
			if element (module_cursor).abbrevation = abbrevation then
				return element (module_cursor);
			end if;

			next (module_cursor);
		end loop;

		-- search without success:
		log_indentation_reset;
		log (message_error & "module abbrevation " & to_string (abbrevation) & " invalid !", console => true);
		raise constraint_error;
	end to_submodule;

	procedure multiple_purpose_error (
	-- Outputs an error message on multiple usage of a purpose of a component category.
		category : in type_component_category; -- CONNECTOR, LIGHT_EMMITTING_DIODE, ...
		purpose : in et_libraries.type_component_purpose.bounded_string) -- PWR_IN, SYS_FAIL, ...
		is
	begin
		log_indentation_reset;
		log (message_error & "There must be ONLY ONE " 
			 & to_string (category) 
			 & " with purpose " 
			 & enclose_in_quotes (et_libraries.to_string (purpose)) & " !",
			 console => true);
		-- CS: show the affected components by reference and coordinates
		raise constraint_error;
	end multiple_purpose_error;
		
	
	function multiple_purpose (
	-- Returns the number of occurences of components with the given purpose and category.
	-- Example: If there are two connectors with purpose "PWR_IN" the return is 2.
		category : in type_component_category; -- CONNECTOR, LIGHT_EMMITTING_DIODE, ...
		purpose : in et_libraries.type_component_purpose.bounded_string; -- PWR_IN, SYS_FAIL, ...
		log_threshold : in et_string_processing.type_log_level)
		return natural is

		occurences : natural := 0; -- to be returned

		use et_coordinates;
		use et_schematic;
		use et_libraries;
		use type_rig;
	
		procedure locate_component (
		-- Searches the component list of the module for a connector with the given purpose.
		-- Exits on the first matching connector. There should not be any others.
			module_name : in type_submodule_name.bounded_string;
			module : in type_module) is
			use et_schematic.type_components;
			use type_component_purpose;
			component : et_schematic.type_components.cursor := module.components.first;
		begin
			log ("detecting multiple usage of purpose " 
				 & enclose_in_quotes (et_libraries.to_string (purpose)) 
				 & " in component category" & to_string (category) 
				 & " ...", log_threshold);
			log_indentation_up;

			while component /= et_schematic.type_components.no_element loop
				if element (component).appearance = sch_pcb then -- it must be a real component
					if et_configuration.category (key (component)) = category then -- category must match
						if element (component).purpose = purpose then -- purpose must match
							log (et_libraries.to_string (key (component)), log_threshold + 1);
							occurences := occurences + 1;
						end if;
					end if;
				end if;
				next (component);
			end loop;

			log_indentation_down;
		end locate_component;

	begin -- multiple_purpose

		query_element (
			position => module_cursor,
			process => locate_component'access);

		-- Show the result of the search:
		if occurences = 0 then
			log_indentation_up;
			log ("none found. very good.", log_threshold + 1);
			log_indentation_down;
		else
			log (message_warning & "for component category " 
				& to_string (category) 
				& " the purpose " 
				& enclose_in_quotes (et_libraries.to_string (purpose)) 
				& " is used multiple times !");
			-- CS: show the affected components by reference and coordinates
		end if;
		
		return occurences;
	end multiple_purpose;
	
		
	procedure validate_module_interconnection (connection : in type_module_interconnection) is
	-- checks if something like "NCC 1 MOTOR_CTRL_OUT_2 MOT 2 MOTOR_CTRL_IN" makes sense
	-- in connection with entries in section import_modules
		use et_coordinates;
		module_A, module_B : type_import_module;

		procedure instance_invalid (
			name : type_submodule_name.bounded_string;
			instance_is, instance_max : type_submodule_instance) is
		begin
			log_indentation_reset;
			log (message_error & "instance index " & to_string (instance_is) 
				& " for submodule " & to_string (name) & " invalid !", console => true);
			log ("Max number of instances specified in section " & section_import_modules & " is " 
				 & to_string (instance_max) & " !", console => true);
			raise constraint_error;
		end instance_invalid;
		
	begin --validate_module_interconnection
		-- load module A/B from the given abbrevation.
		-- Test if given abbrevation is in range of total number of instances for the module.
		module_A := to_submodule (connection.peer_A.abbrevation); -- reason from NCC to "nucleo_core NCC kicad_v4 1"
		if connection.peer_A.instance > module_A.instances then -- instance index check
			instance_invalid (module_A.name, connection.peer_A.instance, module_A.instances);
		end if;
		
		module_B := to_submodule (connection.peer_B.abbrevation); -- reason from MOT to "motor_driver MOT kicad_v4 2"
		if connection.peer_B.instance > module_B.instances then -- instance index check
			instance_invalid (module_B.name, connection.peer_B.instance, module_B.instances);
		end if;
	end validate_module_interconnection;

	function to_connector_reference (
	-- Returns the reference (like X4) of the connector (in the given module) with the given purpose.
	-- Raises error if no connector with given purpose could be found in the module.
		module_name : in et_coordinates.type_submodule_name.bounded_string;
		purpose : in et_libraries.type_component_purpose.bounded_string;
		log_threshold : in type_log_level) 
		return et_libraries.type_component_reference is

		use et_coordinates;
		use et_schematic;
		use et_libraries;
		use type_rig;
		module_cursor : type_rig.cursor;
		connector_found : boolean := false; -- goes true once a suitable connector was found
		ref : et_libraries.type_component_reference; -- the reference to be returned

		procedure locate_component (
		-- Searches the component list of the module for a connector with the given purpose.
		-- Exits on the first matching connector. There should not be any others.
			module_name : in type_submodule_name.bounded_string;
			module : in type_module) is
			use et_schematic.type_components;
			use type_component_purpose;
			component : et_schematic.type_components.cursor := module.components.first;
		begin
			log ("searching connector ...", log_threshold);
			log_indentation_up;

			while component /= et_schematic.type_components.no_element loop
				if element (component).appearance = sch_pcb then -- it must be a real component
					if category (key (component)) = CONNECTOR then -- it must be a connector
						if element (component).purpose = purpose then -- purpose must match
							log ("found -> " & et_libraries.to_string (key (component)), log_threshold);
							connector_found := true;
							ref := key (component);

							-- The connector must be mounted. Otherwise abort.
							if element (component).bom = NO then -- not to be mounted
								log_indentation_reset;
								log (message_error & "connector " & to_string (ref) 
									 & " is NOT supposed for assembly ! Module interconnection not possible !",
									 console => true);
								raise constraint_error;
							end if;
							
							exit;
						end if;
					end if;
				end if;
				next (component);
			end loop;

			-- if no connector was found, the error is raised here:
			if not connector_found then
				log_indentation_reset;
				log (message_error & "module " & to_string (module_name) 
					 & " does not have a" & to_string (connector) & " with purpose "
					 & enclose_in_quotes (et_libraries.to_string (purpose))
					 & " !",
					 console => true);
				raise constraint_error;
			end if;
			
			log_indentation_down;
		end locate_component;
		
	begin -- to_connector_reference
		log ("locating module in rig ...", log_threshold);
		log_indentation_up;

		-- locate the module in the rig
		module_cursor := find (rig, module_name);
		if module_cursor /= no_element then
			query_element (
				position => module_cursor,
				process => locate_component'access);
		else
			-- module does not exist in rig
			log_indentation_reset;
			log (message_error & "module " & to_string (module_name) & " does not exist in the rig !", console => true);
			raise constraint_error;
		end if;

		log_indentation_down;
		return ref;
	end to_connector_reference;

	procedure compare_connector_terminal_count (
	-- Compares the number of terminals of the given connectors.
	-- Raises error if numbers differ.
	-- CS: verificaton required
		module_A		: in et_coordinates.type_submodule_name.bounded_string; -- nucleo_core
		reference_A		: in et_libraries.type_component_reference;				-- X46
		module_B		: in et_coordinates.type_submodule_name.bounded_string;	-- motor_driver
		reference_B		: in et_libraries.type_component_reference;				-- X701
		log_threshold	: in type_log_level) is

		use et_coordinates;
		use et_schematic;
		use et_libraries;
		use type_rig;

		terminal_count_A, terminal_count_B : et_libraries.type_terminal_count;

		procedure module_not_found (module : in et_coordinates.type_submodule_name.bounded_string) is
		begin
			log_indentation_reset;
			log (message_error & "module " & to_string (module) & " not found !",
				 console => true);
			raise constraint_error;
		end module_not_found;
			
	begin -- compare_connector_terminal_count
		log ("comparing connector terminal count ...", log_threshold);
		log_indentation_up;

		-- set the GLOBAL module cursor to module_A
		module_cursor := type_rig.find (rig, module_A);

		-- the module should be found. then get the terminal count of connector A
		if module_cursor /= type_rig.no_element then
			terminal_count_A := terminal_count (reference_A, log_threshold + 2);
			log ("module " & to_string (module_A) & " connector " 
				& to_string (reference_A) & to_string (terminal_count_A),
				log_threshold + 1);
		else -- safety measure in case the module could not be found. should never happen
			module_not_found (module_A);
		end if;

		-- set the GLOBAL module cursor to module_B
		module_cursor := type_rig.find (rig, module_B);
	
		if module_cursor /= type_rig.no_element then
			terminal_count_B := terminal_count (reference_B, log_threshold + 2);
			log ("module " & to_string (module_B) & " connector " 
				& to_string (reference_B) & to_string (terminal_count_B),
				log_threshold + 1);
		else -- safety measure in case the module could not be found. should never happen
			module_not_found (module_B);
		end if;

		-- if terminal counts differ, abort
		if terminal_count_A /= terminal_count_B then
			log_indentation_reset;
			log (message_error & " module " & to_string (module_A) & " connector " & to_string (reference_A)
				 & " and module " & to_string (module_B) & " connector " & to_string (reference_B)
				 & " do not match !",
				 console => true);
			raise constraint_error;
		end if;
				 
		log_indentation_down;
	end compare_connector_terminal_count;


	procedure compare_nets (
	-- Compares net names of the given connectors (via module.netlist).
	-- The net names, the ports and the terminal names on both sides of
	-- the board-to-board connection must be equal.

	-- The workflow in general:
	-- 1. The comparing is conducted first from the right to the left. Means
	-- module_A and reference_A are assumed to be on the right of the connection
	-- while module_B and reference_B are assumed on the left.
	-- 2. In module_right all nets having reference_right are located. Each occurence
	-- stands for the port and terminal (pin/pad) of the connector on the right.
	-- This is based on the netlist of the module (see type_module)
	-- 3. For each port on the right the connector on the left is probed. The port on the left
	-- must be connected to a net with the same name as the one on the right. 
	-- Otherwise a warning is issued or error raised (parameter warn_only).
	-- The left side connector is reference_left which must have the same port and terminal
	-- name. Otherwise a warning is issued or error raised (parameter warn_only). Since the 
	-- terminal names are not in the netlist, they are fetched via the connector reference
	-- by function to_terminal.
	-- 4. The modules A and B swap places. Means module_A and reference_A are assumed to be 
	-- on the LEFT of the connection while module_B and reference_B are assumed on the RIGHT.
	-- Why ? This way open ports are detected.
	-- 5. Steps 2 and 3 are repeated.
	
	-- CS: There could be a time saving approach via the portlists of the modules. The connectors
	-- ports and terminals could be tested for connected nets and compared ...
	
		module_A		: in et_coordinates.type_submodule_name.bounded_string;	-- nucleo_core
		reference_A		: in et_libraries.type_component_reference;				-- X1
		module_B		: in et_coordinates.type_submodule_name.bounded_string;	-- motor_driver
		reference_B		: in et_libraries.type_component_reference;				-- X701
		warn_only		: in type_net_comparator_warn_only;						-- warn or abort on difference
		log_threshold	: in type_log_level) is

		use et_coordinates;
		use et_schematic;
		use et_libraries;
		use type_rig;

		module_cursor_right, module_cursor_left : type_rig.cursor;
		net_right, net_left : type_net_name.bounded_string;	-- motor_on_off
		port_right, port_left : type_port_with_reference;	-- 4
		terminal_right, terminal_left : type_terminal;		-- 4, B3
	
		module_right : type_submodule_name.bounded_string := module_A;	-- nucleo_core
		module_left : type_submodule_name.bounded_string := module_B;	-- motor_driver
		module_swap : type_submodule_name.bounded_string;
	
		reference_right : type_component_reference := reference_A;	-- X1
		reference_left : type_component_reference := reference_B;	-- X701
		reference_swap : type_component_reference;

		procedure query_nets_left (
			module_name : in type_submodule_name.bounded_string;
			module		: in type_module) is
			use type_netlist;
			net_cursor : type_netlist.cursor := module.netlist.first;

			use type_net_name;
			net_found : boolean := false;

			function net_or_terminal_not_found return string is
			begin
				return "module " & to_string (module_left) 
					& " : expect net " & to_string (net_name => net_right)
					& " connected with " & to_string (reference_left)
					& to_string (terminal_right) & "!";
			end net_or_terminal_not_found;
			
			procedure query_ports_left (
				net_name	: in type_net_name.bounded_string;
				ports		: in type_ports_with_reference.set) is
				use type_ports_with_reference;
				use type_port_name;
				port_cursor : type_ports_with_reference.cursor := ports.first;
				terminal_found : boolean := false;
			begin -- query_ports_left
				log_indentation_up;
				log ("locating connector " & to_string (reference => reference_left) 
					& to_string (terminal_right) & "...", log_threshold + 8);

				log_indentation_up;
				while port_cursor /= type_ports_with_reference.no_element loop
					port_left := element (port_cursor);

					if port_left.reference = reference_left then
						log ("connector found", log_threshold + 9);
						
						if port_left.name = port_right.name then
							log ("port found", log_threshold + 9);

							-- fetch terminal name from port_left and current module
							terminal_left := to_terminal (port_left, module_name, log_threshold + 10);

							-- compare terminal names. on match exit loop.
							if terminal_left = terminal_right then
								log ("terminal found", log_threshold + 9);
								terminal_found := true;
								exit;
							end if;
								
						end if;
					end if;
					next (port_cursor);
				end loop;
				log_indentation_down;

				-- If the expected terminal could not be found, issue warning or abort as specified
				-- by input parameter warn_only.
				if not terminal_found then
					case warn_only is
						when ON => 	log (message_warning & net_or_terminal_not_found); 
						when OFF =>	
							log_indentation_reset;
							log (message_error & net_or_terminal_not_found, console => true); 
							raise constraint_error;
					end case;
				end if;
								  
				log_indentation_down;
			end query_ports_left;

		begin -- query_nets_left
			log ("locating net " & to_string (net_name => net_right) 
				& " in module " & to_string (module_left) & " ...", log_threshold + 6);
			log_indentation_up;

			while net_cursor /= type_netlist.no_element loop
				net_left := key (net_cursor);
				log (to_string (net_name => net_left), log_threshold + 7);

				if net_left = net_right then
					net_found := true;
					query_element (
						position => net_cursor,
						process => query_ports_left'access);
					exit;
				end if;

				next (net_cursor);
			end loop;

			-- If expected net not found, issue warning or abort as specified by
			-- input parameter warn_only.
			if not net_found then
				case warn_only is
					when ON => log (message_warning & net_or_terminal_not_found); 
					when OFF =>
						log_indentation_reset;
						log (message_error & net_or_terminal_not_found,
							console => true); 
						raise constraint_error;
				end case;
			end if;

			log_indentation_down;
		end query_nets_left;
		
		procedure query_nets_right (
			module_name : in type_submodule_name.bounded_string;
			module		: in type_module) is
			use type_netlist;
			net_cursor : type_netlist.cursor := module.netlist.first;

			procedure query_ports_right (
				net_name	: in type_net_name.bounded_string;
				ports		: in type_ports_with_reference.set) is
				use type_ports_with_reference;
				port_cursor : type_ports_with_reference.cursor := ports.first;
			begin -- query_ports_right
				net_right := net_name;
				log (to_string (net_right), log_threshold + 3);

				log_indentation_up;				
				log ("querying connector terminals ...", log_threshold + 4);
				log_indentation_up;
				
				-- search for ports that have reference_right
				while port_cursor /= type_ports_with_reference.no_element loop
					if element (port_cursor).reference = reference_right then
						port_right := element (port_cursor);

						-- fetch terminal of port_right
						terminal_right := to_terminal (port_right, module_name, log_threshold + 6);
						
						log (to_string (reference_right)
							& to_string (terminal_right), log_threshold + 5);
						
						log_indentation_up;
						
						-- look up nets in module left
						query_element (
							position => module_cursor_left,
							process => query_nets_left'access);

						log_indentation_down;
					end if;					
					next (port_cursor);
				end loop;
								  
				log_indentation_down;
				log_indentation_down;
			end query_ports_right;
			
		begin -- query_nets_right
			log ("querying nets in module " & to_string (module_right) & " ...", log_threshold + 2);
			log_indentation_up;

			while net_cursor /= type_netlist.no_element loop

				query_element (
					position => net_cursor,
					process => query_ports_right'access);

				next (net_cursor);
			end loop;

			log_indentation_down;			
		end query_nets_right;
		
	begin -- compare_nets
		log ("comparing net names ...", log_threshold);
		log_indentation_up;

		-- Test connection from right to left.
		log ("module right " & to_string (module_right)
			 & " connector right " & to_string (reference_right), log_threshold + 1);

		log ("module left " & to_string (module_left)
			 & " connector left " & to_string (reference_left), log_threshold + 1);

		-- set module cursors		
		module_cursor_right := type_rig.find (rig, module_right);
		module_cursor_left := type_rig.find (rig, module_left);

		log_indentation_up;
		query_element (
			position => module_cursor_right,
			process => query_nets_right'access);
		log_indentation_down;
		
		-- Test connection from left to right
		-- swap places
		module_swap := module_left; -- backup name of module left
		module_left := module_right; -- left becomes right
		module_right := module_swap; -- right becomes left

		reference_swap := reference_left; -- backup name of component reference left
		reference_left := reference_right; -- left becomes right
		reference_right := reference_swap; -- right becomes left

		log ("module right " & to_string (module_right)
			 & " connector right " & to_string (reference_right), log_threshold + 1);

		log ("module left " & to_string (module_left)
			 & " connector left " & to_string (reference_left), log_threshold + 1);
		
		-- set module cursors
		module_cursor_right := type_rig.find (rig, module_right);
		module_cursor_left := type_rig.find (rig, module_left);

		log_indentation_up;
		query_element (
			position => module_cursor_right,
			process => query_nets_right'access);
		log_indentation_down;

		log_indentation_down;

		-- CS: The modules should be found. Place exception handler in case
		-- a module does not exist.
	end compare_nets;
	
	procedure validate_module_interconnections (log_threshold: in et_string_processing.type_log_level) is
	-- Tests if module interconnections at net level make sense.
	-- NOTE: call AFTER modules have been imported !
		use type_module_interconnections;
		use et_coordinates;
		use et_libraries;
		interconnection_cursor		: type_module_interconnections.cursor := module_interconnections.first;
		module_A, module_B			: type_import_module;
		purpose_A, purpose_B 		: et_libraries.type_component_purpose.bounded_string;
		reference_A, reference_B	: et_libraries.type_component_reference;
	begin
		log ("validating module interconnections ...", log_threshold);
		log_indentation_up;
		-- From the module name (nucleo_core) and the purpose (MOTOR_CTRL_OUT_2) of the connector
		-- we reason the references like X1, X46

		-- From the module interconnection like "NCC 1 MOTOR_CTRL_OUT_2 MOT 2 MOTOR_CTRL_IN" we 
		-- reason the full module name and the reference of the connector: 
		-- NCC stands for "nucleo_core" and purpose "MOTOR_CTRL_OUT_2" stands for connector X46.
		while interconnection_cursor /= no_element loop

			-- A map from abbrevation to full module name:
			module_A := to_submodule (element (interconnection_cursor).peer_A.abbrevation);
			log ("module A " & to_string (module_A.name), log_threshold + 2);
			
			-- A map from module name and purpose to reference
			purpose_A := element (interconnection_cursor).peer_A.purpose;
			log ("purpose connector A " & enclose_in_quotes (to_string (purpose_A)), log_threshold + 2);

			reference_A := to_connector_reference (module_A.name, purpose_A, log_threshold + 3);
			log ("reference connector A " & to_string (reference_A), log_threshold + 2);

			
			
			-- B map from abbrevation to full module name:			
			module_B := to_submodule (element (interconnection_cursor).peer_B.abbrevation);
			log ("module B " & to_string (module_B.name), log_threshold + 2);
			
			-- B map from module name and purpose to reference
			purpose_B := element (interconnection_cursor).peer_B.purpose;
			log ("purpose connector B " & enclose_in_quotes (to_string (purpose_B)), log_threshold + 2);
			
			reference_B := to_connector_reference (module_B.name, purpose_B, log_threshold + 3);
			log ("reference connector B " & to_string (reference_B), log_threshold + 2);


			
			-- compare connector terminal counts. each peer must have the same number of terminals
			compare_connector_terminal_count (
				module_A.name, reference_A, -- nucleo_core, X46
				module_B.name, reference_B, -- motor_driver, X701
				log_threshold + 1);
			
			-- compare net names
			-- If net name comparator is turned off for this connection, this step is skipped.
			-- Otherwise net names are compared on both sides of the module interconnection.
			case element (interconnection_cursor).options.comparator is
				when ON =>
					compare_nets (
						module_A => module_A.name,	-- nucleo_core
						reference_A => reference_A, -- X1
						module_B => module_B.name,	-- motor_driver
						reference_B => reference_B,	-- X701
						warn_only => element (interconnection_cursor).options.warn_only, -- warnings or abort on difference
						log_threshold => log_threshold + 1);

				when OFF =>
					log ("net name comparing skipped !", log_threshold + 2);
			end case;
					
			next (interconnection_cursor);
		end loop;

		log_indentation_down;
	end validate_module_interconnections;
	
	function to_string (cat : in type_component_category) return string is
	-- returns the given component category as string
	begin
		return latin_1.space & type_component_category'image (cat);
	end to_string;

	function to_category (category : in string) return type_component_category is
	-- Converts a string to type_component_category.
		use et_string_processing;
		category_out : type_component_category;
	begin
		category_out := type_component_category'value (category);
		return category_out;

		exception
			when others =>
				log_indentation_reset;
				log (message_error & category & " is not a supported component category !",
					 console => true);

				-- CS: show supported categories
				
				raise constraint_error;
	end to_category;
	
	function category (prefix : in et_libraries.type_component_prefix.bounded_string) return
		type_component_category is
	-- Returns the category of the given component prefix. If no category could be
	-- found, returns category UNKNOWN.
		use et_libraries.type_component_prefix;
		use type_component_prefixes;

		prefix_cursor : type_component_prefixes.cursor;
	begin
		-- locate prefix as specified by configuration file
		prefix_cursor := component_prefixes.find (prefix);

		-- If prefix not specified (or no configuration at all) return category UNKNOWN.
		-- Otherwise return the respecitve category.
		if prefix_cursor = type_component_prefixes.no_element then
			log (message_warning & " category of prefix " 
				 & et_libraries.to_string (prefix)
				 & latin_1.space
				 & to_string (UNKNOWN) & " !");
			return UNKNOWN;
		else
			return element (prefix_cursor);
		end if;
	end category;

	
	function category (reference : in et_libraries.type_component_reference) return
		type_component_category is
	-- Returns the category of the given component reference. If no category could be
	-- found, returns category UNKNOWN.
		use et_libraries.type_component_prefix;
		use type_component_prefixes;

		prefix_cursor : type_component_prefixes.cursor;
	begin
		-- locate prefix as specified by configuration file
		prefix_cursor := component_prefixes.find (reference.prefix);

		-- If prefix not specified (or no configuration at all) return category UNKNOWN.
		-- Otherwise return the respecitve category.
		if prefix_cursor = type_component_prefixes.no_element then
			log (message_warning & " category of component " 
				 & et_libraries.to_string (reference)
				 & to_string (UNKNOWN) & " !");
			return UNKNOWN;
		else
			return element (prefix_cursor);
		end if;
	end category;


	function components_in_net (
		module 			: in et_coordinates.type_submodule_name.bounded_string;	-- nucleo_core
		net				: in et_schematic.type_net_name.bounded_string;			-- motor_on_off
		category		: in type_component_category;				-- netchanger, connector
		log_threshold	: in et_string_processing.type_log_level)
		return et_schematic.type_ports_with_reference.set is
	-- Returns a set of component ports that are connected with the given net.
	-- Returns only components of given category.

		use et_libraries;
		use et_schematic;
		use et_coordinates;
		use et_string_processing;
		use type_rig;
		use type_ports_with_reference;

		module_cursor : type_rig.cursor;
		
		ports_all : type_ports_with_reference.set;	-- all ports of the net
		ports_by_category : type_ports_with_reference.set; -- to be returned
		port_cursor : type_ports_with_reference.cursor;
		port_scratch : type_port_with_reference;
		terminal : type_terminal;

	begin -- components_in_net
		log ("locating components of category " & to_string (category) 
			 & " in module " & to_string (module) & " net " & to_string (net) & " ...",
			 log_threshold);
		
		log_indentation_up;

		-- Get all the component ports in the net.
		ports_all := et_schematic.components_in_net (module, net, log_threshold + 2);

		-- If there are ports in the given net, set port cursor to first port in net,
		-- filter ports by category and log ports one after another.
		-- If no ports in net, issue a warning.
		if not is_empty (ports_all) then
			port_cursor := ports_all.first;
			while port_cursor /= type_ports_with_reference.no_element loop
				port_scratch := element (port_cursor); -- load the port

				-- filter by given category and insert the current port_scratch in ports_by_category 
				if et_configuration.category (port_scratch.reference) = category then
					terminal := to_terminal (port_scratch, module, log_threshold + 3); -- fetch the terminal
					log (to_string (port_scratch) & to_string (terminal, show_unit => true, preamble => true),
						log_threshold + 1);

					-- insert in container (to be returned)
					insert (
						container => ports_by_category,
						new_item => port_scratch);
				end if;
				
				next (port_cursor);
			end loop;
		else
			log (message_warning & "net " & to_string (net) & " is not connected with any ports !");
		end if;

		-- show number of component ports that have been found by given category.
		log ("found " & count_type'image (ports_by_category.length) & " ports !", log_threshold + 1);
		
		log_indentation_down;
		return ports_by_category;
	end components_in_net;

	function is_module_interconnector (
	-- Returns true if the given reference in given module is part of a module interconnection.
		module			: in et_coordinates.type_submodule_name.bounded_string;
		reference		: in et_libraries.type_component_reference;
		log_threshold	: in et_string_processing.type_log_level 
		) return boolean is
		use et_libraries;
		use et_coordinates.type_submodule_name;
		use type_module_interconnections;
		connection_cursor : type_module_interconnections.cursor;
		purpose : type_component_purpose.bounded_string;
		result : boolean := false;
	begin
		log ("testing whether component " & to_string (reference) 
			 & " in module " & to_string (module)
			 & " is part of a module interconnection ...", log_threshold);
		log_indentation_up;
		
		-- If there are module interconnections (specified in configuration file section MODULE_INTERCONNECTIONS)
		-- locate the interconnections that address the given module. Other modules are not of interest.
		-- If no module interconnections declared at all, return false.
		if not is_empty (module_interconnections) then
			connection_cursor := module_interconnections.first;
			while connection_cursor /= type_module_interconnections.no_element loop

				-- probe interconnection at peer A. Test if the connector reference at peer A 
				-- matches the the given reference. On match return true.
				if to_submodule (element (connection_cursor).peer_A.abbrevation).name = module then
					purpose := element (connection_cursor).peer_A.purpose;
					if to_connector_reference (module, purpose, log_threshold + 1) = reference then
						result := true;
						exit;
					end if;
				end if;

				-- probe interconnection at peer B. Test if the connector reference at peer B
				-- matches the the given reference. On match return true.				
				if to_submodule (element (connection_cursor).peer_B.abbrevation).name = module then
					purpose := element (connection_cursor).peer_B.purpose;
					if to_connector_reference (module, purpose, log_threshold + 1) = reference then
						result := true;
						exit;
					end if;
				end if;
				
				next (connection_cursor);
			end loop;
		end if;

		-- if result still false, the given reference is not part of a module interconnection
		if result = false then
			log ("no", log_threshold + 1);
		end if;
		
		log_indentation_down;
		return result;
	end is_module_interconnector;
	
	function module_interconnections_in_net (
	-- Returns for the given net the ports which belong to a module interconnection.
	-- If the net is not connected with any module interconnectors the returned list is empty.
	-- If there are no module interactions declared at all, ther returned list is empty.
		module 			: in et_coordinates.type_submodule_name.bounded_string;	-- nucleo_core
		net				: in et_schematic.type_net_name.bounded_string;			-- motor_on_off
		log_threshold	: in et_string_processing.type_log_level)
		return et_schematic.type_ports_with_reference.set is

		use et_schematic;
		use type_ports_with_reference;

		ports_all : type_ports_with_reference.set;
		ports_of_interconnection : type_ports_with_reference.set; -- to be returned
		port_cursor : type_ports_with_reference.cursor;

	begin -- module_interconnections_in_net
		log ("locating module interconnections in net " & to_string (net) & " ...", log_threshold);
		log_indentation_up;

		-- Module interactions are made of connectors only. So we first load ALL connectors
		-- connected with the given net:
		ports_all := components_in_net (module, net, CONNECTOR, log_threshold + 1);

		-- If the net is not connected to any connectors, there is nothing to do. Otherwise
		-- we start looping through ports_all and filter out those connectors which belong
		-- to a module interconnection.
		if not is_empty (ports_all) then
			port_cursor := ports_all.first;
			while port_cursor /= type_ports_with_reference.no_element loop

				-- If port belongs to module interaction, insert it in the list 
				-- ports_of_interconnection (to be returned later).
				if is_module_interconnector (module, element (port_cursor).reference, log_threshold + 1) then
					insert (ports_of_interconnection, element (port_cursor));
				end if;
				
				next (port_cursor);
			end loop;
		end if;

		return ports_of_interconnection;
	end module_interconnections_in_net;
	
	procedure make_routing_tables (log_threshold : in et_string_processing.type_log_level) is
	-- Creates the routing tables for modules and the whole rig.
		use et_string_processing;
		use et_coordinates;
		use et_schematic;
		use type_rig;

		module_cursor : type_rig.cursor;

		procedure query_nets (
			module_name	: in type_submodule_name.bounded_string;
			module		: in type_module) is
			use type_netlist;
			netlist		: type_netlist.map := module.netlist;
			net_cursor	: type_netlist.cursor;
			net_name	: type_net_name.bounded_string;

			netchangers	: type_ports_with_reference.set; -- the netchangers connected with the net
			connectors 	: type_ports_with_reference.set; -- the module interconnectors connected with the net
			
		begin -- query_nets
			if not is_empty (netlist) then
				net_cursor := netlist.first;
				while net_cursor /= type_netlist.no_element loop
					net_name := key (net_cursor);
					netchangers := components_in_net (module_name, net_name, NETCHANGER, log_threshold + 2);
					connectors := module_interconnections_in_net (module_name, net_name, log_threshold + 2);

					-- CS connected_net (module_name, port, log_threshold +x);
					next (net_cursor);
				end loop;
			else
				log (message_warning & "module " & to_string (module_name) & " does not have any nets !");
			end if;
				
		end query_nets;
	
	begin -- make_routing_tables
		log ("making routing tables ...", log_threshold + 1);
		log_indentation_up;

		while module_cursor /= type_rig.no_element loop
			log ("module " & to_string (key (module_cursor)), log_threshold + 1);
			log_indentation_up;

			query_element (
				position => module_cursor,
				process => query_nets'access);
			
			log_indentation_down;
			next (module_cursor);
		end loop;
		
		log_indentation_down;
	end make_routing_tables;

	procedure export_routing_tables is 
	-- CS requires specification. 
	-- CS not called yet from anywhere
	
	-- Exports/Writes the routing tables of the rig in separate files.
	-- Tables are exported in individual project directories in the work directory of ET.
	-- These project directories have the same name as the modules.
	begin
		null; -- CS
	end export_routing_tables;
	
	function to_unit_of_measurement (unit : in string) return type_unit_of_measurement is
	-- Converts a string to type_component_unit_meaning.
		use et_string_processing;
		unit_out : type_unit_of_measurement;
	begin
		unit_out := type_unit_of_measurement'value (unit);
		return unit_out;

		exception
			when others =>
				log_indentation_reset;
				log (message_error & unit & " is not a supported unit of measurement !",
					 console => true);

				-- CS: show supported units
				
				raise constraint_error;
	end to_unit_of_measurement;
	
	function to_string (unit : in type_unit_of_measurement) return string is
	-- returns the given unit of measurement as string. (things like OHM, KILOOHM, MEGAOHM, ...)
	begin
		return latin_1.space & type_unit_of_measurement'image (unit);
	end to_string;
	
	procedure check_abbrevation_of_unit_characters (
		abbrevation : in type_unit_abbrevation.bounded_string;
		characters : in character_set) is
	-- Tests if the given abbrevation contains only valid characters as specified
	-- by given character set. Raises exception if invalid character found.
		use et_string_processing;
		use type_unit_abbrevation;
		invalid_character_position : natural := 0;
	begin
		invalid_character_position := index (
			source => abbrevation,
			set => characters,
			test => outside);

		if invalid_character_position > 0 then
			log_indentation_reset;
			log (message_error & "abbrevaton of unit of measurement " 
				& to_string (abbrevation) 
				& " has invalid character at position"
				& natural'image (invalid_character_position),
				console => true);
			raise constraint_error;
		end if;
	end check_abbrevation_of_unit_characters;

	function requires_operator_interaction (
		prefix : in et_libraries.type_component_prefix.bounded_string) 
		return type_component_requires_operator_interaction is
	-- Returns YES is given prefix requires operator interaction.
		cat : type_component_category;
		use type_categories_with_operator_interacton;		
		cat_cursor : type_categories_with_operator_interacton.cursor;
	begin
		-- get category from given prefix
		cat := category (prefix);

		-- search in container component_categories_with_operator_interaction for
		-- category cat.
		cat_cursor := component_categories_with_operator_interaction.find (cat);

		if cat_cursor = no_element then
			return NO; -- no operator interaction required
		else
			return YES; -- operator interaction required
		end if;
	end requires_operator_interaction;
		
	procedure make_default_configuration (
		file_name		: in type_configuration_file_name.bounded_string;
		log_threshold	: in et_string_processing.type_log_level) is
	-- Creates a default configuration file.
		use et_general;

		function comment return string is begin return comment_mark & latin_1.space; end comment;

	begin
		et_export.create_report;
		reset_warnings_counter;

	
		log ("generating default configuration file " & to_string (file_name), log_threshold);

		if exists (to_string (file_name)) then
			-- CS: warn operator and request confirmation
			null;
		end if;
		
		create (
			file => configuration_file_handle, 
			mode => out_file, 
			name => to_string (file_name));

		put_line (configuration_file_handle, comment & system_name & " configuration");
		put_line (configuration_file_handle, comment & "auto generated by "
			& system_name & latin_1.space & version & " at date " & string (date_now));
		put_line (configuration_file_handle, comment & "Please modify it according to your needs.");
		put_line (configuration_file_handle, comment & row_separator_double);
		new_line (configuration_file_handle);

		-- MODULES TO BE IMPORTED
		put_line (configuration_file_handle, section_import_modules); -- section header
		new_line (configuration_file_handle);		
		put_line (configuration_file_handle, comment & "module abbrevation cad_format [number_of_instances]");
		put_line (configuration_file_handle, comment & "examples:");
		put_line (configuration_file_handle, comment & "nucleo_core NCC kicad_v4 1");
		put_line (configuration_file_handle, comment & "motor_driver MOT kicad_v4 2");
		new_line (configuration_file_handle);		

		-- MODULE INTERCONNECTIONS
		put_line (configuration_file_handle, section_module_interconnections); -- section header
		new_line (configuration_file_handle);		
		put_line (configuration_file_handle, comment 
			& "abbrevation instance connector_purpose abbrevation instance connector_purpose [options]");
		put_line (configuration_file_handle, comment & "examples:"); -- CS more expanation and examples on options
		put_line (configuration_file_handle, comment & "NCC 1 MOTOR_CTRL_OUT_1 MOT 1 MOTOR_CTRL_IN "
			& comment & option_module_interconnections_comparator_off
			& latin_1.space & option_module_interconnections_warn_only);
		
		put_line (configuration_file_handle, comment & "NCC 1 MOTOR_CTRL_OUT_2 MOT 2 MOTOR_CTRL_IN "
			& comment & option_module_interconnections_comparator_off
			& latin_1.space & option_module_interconnections_warn_only);
		new_line (configuration_file_handle);		
		
		-- COMPONENT PREFIXES
		put_line (configuration_file_handle, section_component_prefixes); -- section header
		new_line (configuration_file_handle);		
		put_line (configuration_file_handle, comment & "prefix category");
		new_line (configuration_file_handle);		
		put_line (configuration_file_handle, "ANT" & to_string (ANTENNA)); -- CS: short desciption as comment for all
		put_line (configuration_file_handle, "B  " & to_string (BUZZER));
		put_line (configuration_file_handle, "BAT" & to_string (BATTERY));
		put_line (configuration_file_handle, "C  " & to_string (CAPACITOR));
		put_line (configuration_file_handle, "CA " & to_string (CAPACITOR_ADJUSTABLE));
		--put_line (configuration_file_handle, "CBL" & to_string (CABLE));
		put_line (configuration_file_handle, "D  " & to_string (DIODE));
		put_line (configuration_file_handle, "DPH" & to_string (DIODE_PHOTO));
		put_line (configuration_file_handle, "DI " & to_string (DIAC));
		put_line (configuration_file_handle, "DIS" & to_string (DISPLAY));
		put_line (configuration_file_handle, "F  " & to_string (FUSE));
		put_line (configuration_file_handle, "HS " & to_string (HEATSINK));
		put_line (configuration_file_handle, "IC " & to_string (INTEGRATED_CIRCUIT));
		put_line (configuration_file_handle, "J  " & to_string (JUMPER));
		put_line (configuration_file_handle, "JD " & to_string (JUMPER));
		put_line (configuration_file_handle, "K  " & to_string (RELAY));
		put_line (configuration_file_handle, "KP " & to_string (KEYPAD));
		put_line (configuration_file_handle, "L  " & to_string (INDUCTOR));
		put_line (configuration_file_handle, "LS " & to_string (LOUDSPEAKER));
		put_line (configuration_file_handle, "LED" & to_string (LIGHT_EMMITTING_DIODE));
		put_line (configuration_file_handle, "M  " & to_string (MOTOR));
		put_line (configuration_file_handle, "MIC" & to_string (MICROPHONE));
		put_line (configuration_file_handle, "N  " & to_string (NETCHANGER));
		put_line (configuration_file_handle, "OC " & to_string (OPTOCOUPLER));		
		put_line (configuration_file_handle, "Q  " & to_string (QUARTZ));
		put_line (configuration_file_handle, "R  " & to_string (RESISTOR));
		put_line (configuration_file_handle, "RA " & to_string (RESISTOR_ADJUSTABLE));
		put_line (configuration_file_handle, "RN " & to_string (RESISTOR_NETWORK));
		put_line (configuration_file_handle, "RP " & to_string (POTENTIOMETER));
		put_line (configuration_file_handle, "RPH" & to_string (RESISTOR_PHOTO));
		put_line (configuration_file_handle, "S  " & to_string (SWITCH));
		put_line (configuration_file_handle, "T  " & to_string (TRANSISTOR));
		put_line (configuration_file_handle, "TP " & to_string (TRANSISTOR_PHOTO));
		put_line (configuration_file_handle, "TF " & to_string (TRANSFORMER));
		put_line (configuration_file_handle, "TPT" & to_string (TESTPOINT));
		put_line (configuration_file_handle, "TH " & to_string (THYRISTOR));
		put_line (configuration_file_handle, "THP" & to_string (THYRISTOR_PHOTO));
		put_line (configuration_file_handle, "TR " & to_string (TRIAC));
		put_line (configuration_file_handle, "TUB" & to_string (TUBE));		
		--put_line (configuration_file_handle, "W" & to_string (WIRE));
		put_line (configuration_file_handle, "X  " & to_string (CONNECTOR));
		put_line (configuration_file_handle, "XD " & to_string (CONNECTOR));
		
		new_line (configuration_file_handle);
		new_line (configuration_file_handle);		

		-- UNITS OF COMPONENT VALUES
		put_line (configuration_file_handle, section_component_units); -- section header
		new_line (configuration_file_handle);
		put_line (configuration_file_handle, comment & "abbrevation unit_of_measurement");
		new_line (configuration_file_handle);
		put_line (configuration_file_handle, "m" & to_string (MILLIOHM));
		put_line (configuration_file_handle, "R" & to_string (OHM));
		put_line (configuration_file_handle, "k" & to_string (KILOOHM));
		put_line (configuration_file_handle, "M" & to_string (MEGAOHM));
		put_line (configuration_file_handle, "G" & to_string (GIGAOHM));		

		put_line (configuration_file_handle, "p" & to_string (PICOFARAD));		
		put_line (configuration_file_handle, "n" & to_string (NANOFARAD));
		put_line (configuration_file_handle, "u" & to_string (MICROFARAD));
		put_line (configuration_file_handle, "m" & to_string (MILLIFARAD));
		put_line (configuration_file_handle, "F" & to_string (FARAD));

		put_line (configuration_file_handle, "n" & to_string (NANOHENRY));		
		put_line (configuration_file_handle, "u" & to_string (MICROHENRY));
		put_line (configuration_file_handle, "m" & to_string (MILLIHENRY));
		put_line (configuration_file_handle, "H" & to_string (HENRY));

		put_line (configuration_file_handle, "V" & to_string (VOLT));

		put_line (configuration_file_handle, "m" & to_string (MILLIAMPERE));		
		put_line (configuration_file_handle, "A" & to_string (AMPERE));

		put_line (configuration_file_handle, "k" & to_string (KILOHERTZ));
		put_line (configuration_file_handle, "M" & to_string (MEGAHERTZ));
		put_line (configuration_file_handle, "G" & to_string (GIGAHERTZ));
		
		new_line (configuration_file_handle);
		new_line (configuration_file_handle);		

		-- COMPONENT THAT REQUIRE OPERATOR INTERACTION
		put_line (configuration_file_handle, section_components_with_operator_interaction); -- section header
		new_line (configuration_file_handle);		
		put_line (configuration_file_handle, comment & "category");
		new_line (configuration_file_handle);		
		put_line (configuration_file_handle, to_string (BUZZER));
		put_line (configuration_file_handle, to_string (CAPACITOR_ADJUSTABLE));
		put_line (configuration_file_handle, to_string (CONNECTOR));
		put_line (configuration_file_handle, to_string (DIODE_PHOTO));
		put_line (configuration_file_handle, to_string (DISPLAY));
		put_line (configuration_file_handle, to_string (FUSE));
		put_line (configuration_file_handle, to_string (JUMPER));
		put_line (configuration_file_handle, to_string (KEYPAD));
		put_line (configuration_file_handle, to_string (LIGHT_EMMITTING_DIODE));
		put_line (configuration_file_handle, to_string (RESISTOR_ADJUSTABLE));
		put_line (configuration_file_handle, to_string (RESISTOR_PHOTO));
		put_line (configuration_file_handle, to_string (POTENTIOMETER));
		put_line (configuration_file_handle, to_string (SWITCH));
		put_line (configuration_file_handle, to_string (TESTPOINT));
		put_line (configuration_file_handle, to_string (THYRISTOR_PHOTO));

		new_line (configuration_file_handle);
		

		-- CONNECTOR GND TERMINAL
        -- CS
-- 		put_line (configuration_file_handle, section_connector_gnd_terminal); -- section header
-- 		new_line (configuration_file_handle);

		-- CS LINE WIDTHS

		-- CS TEXT SIZES
	
		
		put_line (configuration_file_handle, comment & row_separator_double);		
		put_line (configuration_file_handle, comment & system_name & " configuration end");
		close (configuration_file_handle);

		et_export.close_report;

		exception
			when event:
				others => 
					et_export.close_report;
					put_line (standard_output, message_error & "Read export report for warnings and error messages !"); -- CS: show path to report file

		
	end make_default_configuration;


	procedure read_configuration (
		file_name		: in type_configuration_file_name.bounded_string;
		log_threshold	: in et_string_processing.type_log_level) is
	-- Reads the given configuration file.
	-- Fills component_prefixes.

		use et_string_processing;
		line : et_string_processing.type_fields_of_line; -- the line of the file

		function field (line : in type_fields_of_line; position : in positive) return string renames
			et_string_processing.get_field_from_line;

		type type_section is (
			none,
			import_modules,
			module_interconnections,
			component_prefixes,
			component_units,
			components_with_operator_interaction
			);
		
		section_entered : type_section := none;
		
		-- lines of the file are collected in a simple list:
		package type_lines is new doubly_linked_lists (
			element_type => et_string_processing.type_fields_of_line,
			"=" => et_string_processing.lines_equally);
		use type_lines;
		lines : type_lines.list := type_lines.empty_list;
			
		procedure process_previous_section is
		-- Processes the section indicated by section_entered. 
		-- The lines of the section are in container "lines".
		-- Clears "lines" after processing.
			line_cursor : type_lines.cursor := lines.first; -- points to the line being processed
			component_prefix_cursor : type_component_prefixes.cursor; -- CS: rename to prefix_cursor
			unit_cursor : type_units_of_measurement.cursor;
			inserted : boolean := false;

			-- we deal with columns and need to index them
			subtype type_column is positive range 1..8;
			column_module_name	: type_column;
			column_cad_format	: type_column;
			column_abbrevation_A,	column_abbrevation_B	: type_column;
			column_instance_A, 		column_instance_B		: type_column;		
			column_purpose_A,		column_purpose_B		: type_column;
			column_first_option	: type_column;
		
			use et_libraries;
			use et_coordinates;
			use type_import_modules;

			procedure test_multiple_occurences is 
			begin
				if not inserted then
					log (message_warning & affected_line (element (line_cursor)) & "multiple occurence of assignment ! Entry ignored !");
				end if;
			end test_multiple_occurences;

			procedure test_multiple_occurences (module : in type_import_module) is
			-- Tests if given module is already in the import_modules list.
				module_cursor : type_import_modules.cursor := et_configuration.import_modules.first;
				use type_submodule_name;
			begin
				while module_cursor /= type_import_modules.no_element loop
					if element (module_cursor).name = module.name then
						log_indentation_reset;
						log (message_error & affected_line (element (line_cursor)) 
								& "duplicated entry !",
								console => true);
						raise constraint_error;
					end if;
					next (module_cursor);
				end loop;
			end test_multiple_occurences;

			procedure test_multiple_occurences (connection : in type_module_interconnection) is
			-- Tests if given module interconnection uses a connector that is already used.
				use type_module_interconnections;
				connection_cursor : type_module_interconnections.cursor;
				connection_scratch : type_module_interconnection;
			
				procedure connector_already_used (connector : in type_connector) is
				begin
					log_indentation_reset;
					log (message_error & "connector " & to_string (connector.abbrevation) 
						 & " instance " & to_string (connector.instance)
						 & " already used !",
						 console => true);
					raise constraint_error;
				end connector_already_used;
			
			begin -- test_multiple_occurences

				-- This test does only make sense if any connection have been read already.
				-- If there are connections, we test if the given connections uses a connector
				-- that has been used already.
				if not is_empty (et_configuration.module_interconnections) then
					connection_cursor := et_configuration.module_interconnections.first;

					-- loop through connections 
					while connection_cursor /= type_module_interconnections.no_element loop
						connection_scratch := element (connection_cursor);

						-- test peer A
						if 	connection_scratch.peer_A = connection.peer_A or 
							connection_scratch.peer_A = connection.peer_B then
							connector_already_used (connection_scratch.peer_A);
						end if;

						-- test peer B
						if 	connection_scratch.peer_B = connection.peer_A or 
							connection_scratch.peer_B = connection.peer_B then
							connector_already_used (connection_scratch.peer_B);
						end if;
						
						next (connection_cursor);
					end loop;
				end if;
			end test_multiple_occurences;

			
			module		: type_import_module;
			connection	: type_module_interconnection;
			
			prefix 		: type_component_prefix.bounded_string;
			cat 		: type_component_category;
			
			abbrevation	: type_unit_abbrevation.bounded_string;
			unit		: type_unit_of_measurement;
			
			-- CS: check field count in sections respectively. issue warning if too many fields. 
		begin -- process_previous_section
			next (line_cursor); -- the first line of the section is its header itself and can be skipped
			log_indentation_up;

			case section_entered is
				when none => null;

				-- MODULES TO BE IMPORTED
				when import_modules =>
					log ("import modules ...", log_threshold + 1);
					log_indentation_up;

					-- Mind table header in conf. file when changing anything here.
					-- See procedure make_default_configuration.
					column_module_name		:= 1; 
					column_abbrevation_A	:= 2;
					column_cad_format		:= 3;
					column_instance_A		:= 4;
					
					while line_cursor /= type_lines.no_element loop
						log (to_string (element (line_cursor)), log_threshold + 2);

						-- read module name
						check_submodule_name_length (field (element (line_cursor), column_module_name));
						module.name := to_submodule_name (field (element (line_cursor), column_module_name));
						check_submodule_name_characters (module.name);
						et_import.validate_project (et_schematic.to_project_name (field (element (line_cursor), column_module_name)));

						-- read abbrevation
						check_submodule_abbrevation_length (field (element (line_cursor), column_abbrevation_A));
						module.abbrevation := to_abbrevation (field (element (line_cursor), column_abbrevation_A));
						check_submodule_abbrevation_characters (module.abbrevation);

						-- read cad format
						et_import.validate_cad_format (field (element (line_cursor), column_cad_format));
						module.format := et_import.to_cad_format (field (element (line_cursor), column_cad_format));
						-- CS: default if not provided ?

						-- read number of instances
						check_number_of_instances (field (element (line_cursor), column_instance_A)); -- character check included
						module.instances := to_number_of_instances (field (element (line_cursor), column_instance_A));
						-- CS: default if not provided ?

						-- test multiple occurences of module name
						test_multiple_occurences (module);

						-- insert module in container
						type_import_modules.append (
							container => et_configuration.import_modules,
							new_item => module);
						
						next (line_cursor);
					end loop;
					log_indentation_down;

				-- MODULE INTERCONNECTIONS
				when module_interconnections =>
					log ("module interconnections ...", log_threshold + 1);
					log_indentation_up;

					-- Mind table header in conf. file when changing anything here.
					-- See procedure make_default_configuration.
					column_abbrevation_A	:= 1;
					column_instance_A		:= 2;
					column_purpose_A		:= 3;
					column_abbrevation_B	:= 4;
					column_instance_B		:= 5;
					column_purpose_B		:= 6;
					column_first_option		:= 7;

					-- we read a line like "# NCC 1 MOTOR_CTRL_OUT_1 MOT 1 MOTOR_CTRL_IN"
					while line_cursor /= type_lines.no_element loop
						log (to_string (element (line_cursor)), log_threshold + 2);

						-- read module abbrevation A
						check_submodule_abbrevation_length (field (element (line_cursor), column_abbrevation_A));
						connection.peer_A.abbrevation := to_abbrevation (field (element (line_cursor), column_abbrevation_A));
						check_submodule_abbrevation_characters (connection.peer_A.abbrevation);

						-- read number of instances
						check_number_of_instances (field (element (line_cursor), column_instance_A)); -- character check included
						connection.peer_A.instance := to_number_of_instances (field (element (line_cursor), column_instance_A));
						
						-- read connector purpose A
						check_purpose_length (field (element (line_cursor), column_purpose_A));
						validate_purpose (field (element (line_cursor), column_purpose_A));
						connection.peer_A.purpose := to_purpose (field (element (line_cursor), column_purpose_A));
						check_purpose_characters (connection.peer_A.purpose);
						-- NOTE: The question whether there is a connector with this purpose in the module can
						-- not be answered here, because the project has not been imported yet.
						
						-- read module abbrevation B
						check_submodule_abbrevation_length (field (element (line_cursor), column_abbrevation_B));
						connection.peer_B.abbrevation := to_abbrevation (field (element (line_cursor), column_abbrevation_B));
						check_submodule_abbrevation_characters (connection.peer_B.abbrevation);

						-- read number of instances
						check_number_of_instances (field (element (line_cursor), column_instance_B)); -- character check included
						connection.peer_B.instance := to_number_of_instances (field (element (line_cursor), column_instance_B));
						
						-- read connector purpose B
						check_purpose_length (field (element (line_cursor), column_purpose_B));
						validate_purpose (field (element (line_cursor), column_purpose_B));
						connection.peer_B.purpose := to_purpose (field (element (line_cursor), column_purpose_B));
						check_purpose_characters (connection.peer_B.purpose);
						-- NOTE: The question whether there is a connector with this purpose in the module can
						-- not be answered here, because the project has not been imported yet.

						-- read options 
						-- If any provided the number of fields is greater thatn column_first_option).
						-- example: LMX 1 "PWR CTRL IN" PWR 1 "PWR CTRL OUT" net_comparator_off #warn_only
						-- CS: If mutually exclusive options given, then the last one overrides the previous.
						if field_count (element (line_cursor)) >= count_type (column_first_option) then
							log ("options provided", log_threshold + 1);
							log_indentation_up;

							-- Loop in options and test for options. Once a supported option occurs, set the
							-- corresponding option in the connection being built.
							for pos in positive (column_first_option) .. positive (field_count (element (line_cursor))) loop

								-- test option keywords
								if field (element (line_cursor), pos) = option_module_interconnections_comparator_off then
									connection.options.comparator := OFF;
								elsif field (element (line_cursor), pos) = option_module_interconnections_comparator_on then
									connection.options.comparator := ON;
								elsif field (element (line_cursor), pos) = option_module_interconnections_warn_only then
									connection.options.warn_only := ON;
								else
									log_indentation_reset;
									log (message_error & "unknown option " & field (element (line_cursor), pos), console => true);
									raise constraint_error;
								end if;
								
							end loop;

							log (to_string (connection.options.comparator), log_threshold + 1);
							log (to_string (connection.options.warn_only), log_threshold + 1);

							-- comparator OFF and warnings only at the same time does not make sense:
							if connection.options.comparator = OFF and connection.options.warn_only = ON then
								log_indentation_reset;
								log (message_error & "mutually exclusive options : " 
									& to_string (connection.options.comparator) & " and "
									& to_string (connection.options.warn_only) & " does not make sense !",
									console => true);
								raise constraint_error;
							end if;
							

							log_indentation_down;
						end if;

						-- test multiple occurences of connection
						test_multiple_occurences (connection);

						-- check if all this makes sense in connection with entries in section import_modules
						validate_module_interconnection (connection);
						
						-- insert interconnection in container
						type_module_interconnections.append (
							container => et_configuration.module_interconnections,
							new_item => connection);
						
						next (line_cursor);
					end loop;
					log_indentation_down;

				-- COMPONENT PREFIXES
				when component_prefixes =>
					log ("component prefixes ...", log_threshold + 1);
					log_indentation_up;

					while line_cursor /= type_lines.no_element loop
						log (to_string (element (line_cursor)), log_threshold + 2);

						-- Build the prefix from field #1:
						-- Test if prefix is not too long, if it contains only allowed characters.
						-- We test against the default character set as specified in et_libraries.
						check_prefix_length (field (element (line_cursor), 1));
						prefix := type_component_prefix.to_bounded_string (field (element (line_cursor), 1));
						check_prefix_characters (prefix, component_prefix_characters);

						-- build the component category from field #2:
						cat := to_category (field (element (line_cursor), 2));
						
						-- insert the prefix assignment in container component_prefixes
						type_component_prefixes.insert (
							container => et_configuration.component_prefixes,
							position => component_prefix_cursor,
							key => prefix,
							new_item => cat,
							
							-- If entry already in map, this flag goes true. Warning issued later. see below.
							inserted => inserted);

						test_multiple_occurences;
						next (line_cursor);
					end loop;
					log_indentation_down;

				-- COMPONENT UNITS OF MEASUREMENT
				when component_units =>
					log ("component units of measurement ...", log_threshold + 1);
					log_indentation_up;
					while line_cursor /= type_lines.no_element loop
						log (to_string (element (line_cursor)), log_threshold + 2);

						-- Build the unit abbrevation from field #1:
						-- Test if abbrevation contains only allowed characters.
						-- We test against the character set specified for abbrevations of units of measurement.
						abbrevation := type_unit_abbrevation.to_bounded_string (field (element (line_cursor), 1));
						check_abbrevation_of_unit_characters (abbrevation, unit_abbrevation_characters);

						-- Build the unit of measurement from field #2:
						unit := to_unit_of_measurement (field (element (line_cursor), 2));
						
						-- insert the abbrevation to unit of measurement assignment in container component_units
						type_units_of_measurement.insert (
							container => et_configuration.component_units,
							position => unit_cursor,

							-- If entry already in map, this flag goes true. Warning issued later. see below.
							inserted => inserted,

							-- the key in this map is the unit of measurement like MICROFARAD or NANOHENRY
							key => unit,
							
							-- the item is the abbrevation of unit of measurement
							new_item => abbrevation
							);

						test_multiple_occurences;
						next (line_cursor);
					end loop;
					log_indentation_down;

				-- COMPONENTS WITH USER INTERACTON
				when components_with_operator_interaction =>
					log ("component categories with operator interaction ...", log_threshold + 1);
					log_indentation_up;
					while line_cursor /= type_lines.no_element loop
						log (to_string (element (line_cursor)), log_threshold + 2);

						-- build the component category from field #1:
						cat := to_category (field (element (line_cursor), 1));

						-- insert the category in container component_categories_with_operator_interaction
						type_categories_with_operator_interacton.insert (
							container => component_categories_with_operator_interaction,
							new_item => cat);
						
						next (line_cursor);
					end loop;
					log_indentation_down;

					
			end case;
			
			log_indentation_down;

			-- clean up. empty container "lines" for next section
			lines.clear;

			exception
				when others =>
					log_indentation_reset;
					log (message_error & affected_line (element (line_cursor)) & latin_1.space & to_string (element (line_cursor)),
						 console => true);

					-- CS: provide information on what is wrong with the line (depending on section_entered)
					-- number of characters in unit of measurement
					-- propose category, units, ...
					
					raise;
					
		end process_previous_section;
		
	begin -- read_configuration
		log ("reading configuration file " & to_string (file_name) & " ...", log_threshold);

		if exists (to_string (file_name)) then

			et_configuration.component_prefixes := type_component_prefixes.empty_map;

			open (file => configuration_file_handle, mode => in_file, name => to_string (file_name));
			set_input (configuration_file_handle);

			-- read configuration file line per line
			while not end_of_file loop

				-- Store line in variable "line" (see et_string_processing.ads)
				line := et_string_processing.read_line (
					line => get_line,
					number => ada.text_io.line (current_input),
					comment_mark => et_general.comment_mark,
					delimiter_wrap => true, -- if connector purpose is given in quotations
					ifs => latin_1.space); -- fields are separated by space

				case field_count (line) is
					when 0 => null; -- we skip empty lines
					when others =>

						-- CS: check field count ?
						
						-- At a certain log level we report the whole line as it is:
						--log (to_string (line), log_threshold + 3);

						-- Wait for a section header.
						-- Once a header was found, the PREVIOUS section is regarded as complete.
						-- The PREVIOUS section is then processed with all its lines in container "lines".
						-- Set section_entered according to the section BEING entered.
						if field (line, 1) = section_import_modules then
							process_previous_section;
							section_entered := import_modules;
						end if;

						if field (line, 1) = section_module_interconnections then
							process_previous_section;
							section_entered := module_interconnections;
						end if;
						
						if field (line, 1) = section_component_prefixes then
							process_previous_section;
							section_entered := component_prefixes;
						end if;

						if field (line, 1) = section_component_units then
							process_previous_section;
							section_entered := component_units;
						end if;

						if field (line, 1) = section_components_with_operator_interaction then
							process_previous_section;
							section_entered := components_with_operator_interaction;
						end if;

						-- CS: place other sections header tests here
						
						-- For all entered sections collect lines in container "lines".
						case section_entered is
							when none => null;
							when others =>
								lines.append (line);
						end case;
						
				end case;
				
			end loop;

			-- The last section of the file is complete, once the file end is reached.
			process_previous_section;
			
			set_input (standard_input);
			close (configuration_file_handle);
			
		else
			log_indentation_reset;
			log (message_error & "configuration file " & to_string (file_name) & " not found !",
				 console => true);
			raise constraint_error;
		end if;

	end read_configuration;

	procedure validate_prefix (prefix : in et_libraries.type_component_prefix.bounded_string) is
	-- Tests if the given prefix is a power_flag_prefix or a power_symbol_prefix.
	-- Raises exception if not.
		use et_libraries.type_component_prefix;
		use type_component_prefixes;
	
		prefix_cursor : type_component_prefixes.cursor;
	begin
		if component_prefixes.find (prefix) /= type_component_prefixes.no_element then
			null;
		else
			log_indentation_reset;
			log (message_error & "invalid prefix "
				 & to_string (prefix) & " !"
				 & " See configuration file for valid prefixes.",
				console => true
				);
			raise constraint_error;
		end if;
	end validate_prefix;
	
	procedure validate_prefix (reference : in et_libraries.type_component_reference) is
	-- Tests if the given reference has a valid prefix as specified in the configuration file.
	-- Raises exception if not.
		use et_libraries.type_component_prefix;
		use type_component_prefixes;
	begin
		if component_prefixes.find (reference.prefix) /= type_component_prefixes.no_element then
			null;
		else
			log_indentation_reset;
			log (message_error & "invalid prefix in component reference "
				 & et_libraries.to_string (reference) & " !"
				 & " See configuration file for valid prefixes.",
				console => true
				);
			-- CS: show coordinates of affected component
			raise constraint_error;
		end if;
	end validate_prefix;

	
end et_configuration;

-- Soli Deo Gloria
