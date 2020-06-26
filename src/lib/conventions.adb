------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                             CONVENTIONS                                  --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2020 Mario Blunk, Blunk electronic          --
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

--   For correct displaying set tab width in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;
with ada.strings; 				use ada.strings;
--with ada.strings.maps;			use ada.strings.maps;
with ada.strings.maps.constants;

-- with ada.characters.handling;	use ada.characters.handling;
with ada.strings.fixed; 		use ada.strings.fixed;

with ada.exceptions;

with et_general;

with et_project;
with et_export;
with et_import;
with et_csv;

package body conventions is

	use et_string_processing;
	
-- 	function to_string (net_comparator_on_off : in type_net_comparator_on_off) return string is
-- 	-- Returns the given net comparator status as string.
-- 	begin
-- 		return "net name comparator " & type_net_comparator_on_off'image (net_comparator_on_off);
-- 	end to_string;

-- 	function to_string (net_comparator_warn : in type_net_comparator_warn_only) return string is
-- 	-- Returns the given net comparator warning status as string.
-- 	begin
-- 		return "warnings only " & type_net_comparator_warn_only'image (net_comparator_warn);
-- 	end to_string;
-- 	
-- 	function to_submodule (abbrevation : in et_coordinates.type_submodule_abbrevation.bounded_string) 
-- 		return type_import_module is
-- 	-- Looks up the container import_modules for the given abbrevation and returns the submodule.
-- 	-- Raises alarm if no submodule could be found -> abbrevation invalid.	
-- 		use et_coordinates.type_submodule_abbrevation;
-- 		use type_import_modules;
-- 		module_cursor : type_import_modules.cursor := import_modules.first;
-- 	begin
-- 		while module_cursor /= type_import_modules.no_element loop
-- 			if element (module_cursor).abbrevation = abbrevation then
-- 				return element (module_cursor);
-- 			end if;
-- 
-- 			next (module_cursor);
-- 		end loop;
-- 
-- 		-- search without success:
-- 		log_indentation_reset;
-- 		log (message_error & "module abbrevation " & to_string (abbrevation) & " invalid !", console => true);
-- 		raise constraint_error;
-- 	end to_submodule;

-- 	function to_abbrevation (module_name : in et_coordinates.type_submodule_name.bounded_string) 
-- 		return et_coordinates.type_submodule_abbrevation.bounded_string is
-- 	-- Looks up the container import_modules for the given module name and returns the abbrevation.
-- 	-- Raises alarm if no submodule could be found -> modue name invalid.
-- 		use et_coordinates.type_submodule_name;
-- 		use type_import_modules;
-- 		module_cursor : type_import_modules.cursor := import_modules.first;
-- 	begin
-- 		while module_cursor /= type_import_modules.no_element loop
-- 			if element (module_cursor).name = module_name then
-- 				return element (module_cursor).abbrevation;
-- 			end if;
-- 
-- 			next (module_cursor);
-- 		end loop;
-- 
-- 		-- search without success:
-- 		log_indentation_reset;
-- 		log (message_error & "module " & to_string (module_name) & " invalid !", console => true);
-- 		raise constraint_error;
-- 	end to_abbrevation;
	
		
-- 	procedure validate_module_interconnection (connection : in type_module_interconnection) is
-- 	-- checks if something like "NCC 1 MOTOR_CTRL_OUT_2 MOT 2 MOTOR_CTRL_IN" makes sense
-- 	-- in connection with entries in section import_modules
-- 		use et_coordinates;
-- 		module_A, module_B : type_import_module;
-- 
-- 		procedure instance_invalid (
-- 			name : type_submodule_name.bounded_string;
-- 			instance_is, instance_max : type_submodule_instance) is
-- 		begin
-- 			log_indentation_reset;
-- 			log (message_error & "instance index " & to_string (instance_is) 
-- 				& " for submodule " & to_string (name) & " invalid !", console => true);
-- 			log ("Max number of instances specified in section " & section_import_modules & " is " 
-- 				 & to_string (instance_max) & " !", console => true);
-- 			raise constraint_error;
-- 		end instance_invalid;
-- 		
-- 	begin --validate_module_interconnection
-- 		-- load module A/B from the given abbrevation.
-- 		-- Test if given abbrevation is in range of total number of instances for the module.
-- 		module_A := to_submodule (connection.peer_A.abbrevation); -- reason from NCC to "nucleo_core NCC kicad_v4 1"
-- 		if connection.peer_A.instance > module_A.instances then -- instance index check
-- 			instance_invalid (module_A.name, connection.peer_A.instance, module_A.instances);
-- 		end if;
-- 		
-- 		module_B := to_submodule (connection.peer_B.abbrevation); -- reason from MOT to "motor_driver MOT kicad_v4 2"
-- 		if connection.peer_B.instance > module_B.instances then -- instance index check
-- 			instance_invalid (module_B.name, connection.peer_B.instance, module_B.instances);
-- 		end if;
-- 	end validate_module_interconnection;

-- 	function to_connector_reference (
-- 	-- Returns the reference (like X4) of the connector in the given generic module with 
-- 	-- given instance with the given purpose.
-- 	-- Raises error if connector could be found.
-- 		generic_module_name	: in et_coordinates.type_submodule_name.bounded_string;		-- led_matrix
-- 		instance			: in et_coordinates.type_submodule_instance;				-- 1
-- 		purpose				: in et_libraries.type_component_purpose.bounded_string;	-- "PWR CTRL IN"
-- 		log_threshold		: in type_log_level) 
-- 		return et_libraries.type_name is
-- 
-- 		use et_coordinates;
-- 		--use et_schematic;
-- 		use et_libraries;
-- 		use et_kicad.type_rig;
-- 		module_cursor : et_kicad.type_rig.cursor;
-- 		module_found : boolean := false;
-- 		connector_found : boolean := false; -- goes true once a suitable connector was found
-- 		ref : et_libraries.type_name; -- the reference to be returned
-- 
-- 		procedure locate_component (
-- 		-- Searches the component list of the module for a connector with the given purpose.
-- 		-- Exits on the first matching connector. There should not be any others.
-- 			module_name	: in type_submodule_name.bounded_string;
-- 			module		: in et_kicad.type_module) is
-- 			use et_kicad.type_components_schematic;
-- 			use type_component_purpose;
-- 			component : et_kicad.type_components_schematic.cursor := module.components.first;
-- 		begin
-- 			log ("searching connector ...", log_threshold);
-- 			log_indentation_up;
-- 
-- 			while component /= et_kicad.type_components_schematic.no_element loop
-- 				if element (component).appearance = sch_pcb then -- it must be a real component
-- 					if category (key (component)) = CONNECTOR then -- it must be a connector
-- 						if element (component).purpose = purpose then -- purpose must match
-- 							log ("found -> " & et_libraries.to_string (key (component)), log_threshold);
-- 							connector_found := true;
-- 							ref := key (component);
-- 
-- 							-- The connector must be mounted. Otherwise abort.
-- 							if element (component).bom = NO then -- not to be mounted
-- 								log_indentation_reset;
-- 								log (message_error & "connector " & to_string (ref) 
-- 									 & " is NOT supposed for assembly ! Module interconnection not possible !",
-- 									 console => true);
-- 								raise constraint_error;
-- 							end if;
-- 							
-- 							exit;
-- 						end if;
-- 					end if;
-- 				end if;
-- 				next (component);
-- 			end loop;
-- 
-- 			-- if no connector was found, the error is raised here:
-- 			if not connector_found then
-- 				log_indentation_reset;
-- 				log (message_error & "module " & to_string (module_name) 
-- 					 & " does not have a " & to_string (connector) & " with purpose "
-- 					 & enclose_in_quotes (et_libraries.to_string (purpose))
-- 					 & " !",
-- 					 console => true);
-- 				
-- 				log ("Make sure prefixes are specified in configuration file section "
-- 					 & section_component_prefixes & " !");
-- 
-- 				raise constraint_error;
-- 			end if;
-- 			
-- 			log_indentation_down;
-- 		end locate_component;
-- 
-- 		use type_submodule_name;
-- 		
-- 	begin -- to_connector_reference
-- 		log ("locating module " & to_string (submodule => generic_module_name) & " in rig ...", log_threshold);
-- 		log_indentation_up;
-- 
-- 		-- locate the module in the rig by its generic name and instance
-- 		module_cursor := et_kicad.rig.first;
-- 		while module_cursor /= no_element loop
-- 			if element (module_cursor).generic_name = generic_module_name then
-- 				if element (module_cursor).instance = instance then
-- 					query_element (
-- 						position => module_cursor,
-- 						process => locate_component'access);
-- 					module_found := true;
-- 					exit;
-- 				end if;
-- 			end if;
-- 			next (module_cursor);
-- 		end loop;
-- 
-- 		if not module_found then
-- 			log_indentation_reset;
-- 			log (message_error & "no generic module " & to_string (submodule => generic_module_name)
-- 				 & " with instance " & et_coordinates.to_string (instance)
-- 				 & " found in the rig !", console => true);
-- 			raise constraint_error;
-- 		end if;
-- 
-- 		log_indentation_down;
-- 		return ref;
-- 	end to_connector_reference;

-- 	procedure compare_connector_terminal_count (
-- 	-- Compares the number of terminals of the given connectors.
-- 	-- Raises error if numbers differ.
-- 	-- CS: verificaton required
-- 		module_A		: in et_coordinates.type_submodule_name.bounded_string; -- generic name like nucleo_core
-- 		instance_A		: in et_coordinates.type_submodule_instance;			-- 1
-- 		reference_A		: in et_libraries.type_name;				-- X46
-- 		module_B		: in et_coordinates.type_submodule_name.bounded_string;	-- generic name like motor_driver
-- 		instance_B		: in et_coordinates.type_submodule_instance;			-- 4
-- 		reference_B		: in et_libraries.type_name;				-- X701
-- 		log_threshold	: in type_log_level) is
-- 
-- 		use et_coordinates;
-- 		--use et_schematic;
-- 		use et_coordinates.type_submodule_name;
-- 		use et_libraries;
-- 		use et_kicad.type_rig;
-- 
-- 		module_found : boolean := false;
-- 	
-- 		terminal_count_A, terminal_count_B : et_libraries.type_terminal_count;
-- 
-- 		procedure module_not_found (
-- 			name		: in et_coordinates.type_submodule_name.bounded_string;
-- 			instance	: in et_coordinates.type_submodule_instance) is
-- 		begin
-- 			log_indentation_reset;
-- 			log (message_error & "module " & to_string (submodule => name) 
-- 				& " instance " & et_coordinates.to_string (instance) & " not found !",
-- 				console => true);
-- 			raise constraint_error;
-- 		end module_not_found;
-- 			
-- 	begin -- compare_connector_terminal_count
-- 		log ("comparing connector terminal count ...", log_threshold);
-- 		log_indentation_up;
-- 
-- 		-- locate module A in the rig by its generic name and instance
-- 		-- CS probe CAD format
-- 		et_kicad.module_cursor := et_kicad.type_rig.first (et_kicad.rig);
-- 		while et_kicad.module_cursor /= et_kicad.type_rig.no_element loop
-- 			if element (et_kicad.module_cursor).generic_name = module_A then
-- 				if element (et_kicad.module_cursor).instance = instance_A then
-- 					-- get the terminal count of connector A
-- 					terminal_count_A := et_kicad.terminal_count (reference_A, log_threshold + 2);
-- 					log ("module " & to_string (submodule => module_A) & " instance " 
-- 						& et_coordinates.to_string (instance_A) & " connector " 
-- 						& to_string (reference_A) & to_string (terminal_count_A),
-- 						log_threshold + 1);
-- 					module_found := true;
-- 					exit;
-- 				end if;
-- 			end if;
-- 			next (et_kicad.module_cursor);
-- 		end loop;
-- 
-- 		if not module_found then -- safety measure in case the module could not be found. should never happen
-- 			module_not_found (module_A, instance_A);
-- 		end if;
-- 
-- 		
-- 		-- locate module B in the rig by its generic name and instance
-- 		et_kicad.module_cursor := et_kicad.type_rig.first (et_kicad.rig);
-- 		while et_kicad.module_cursor /= et_kicad.type_rig.no_element loop
-- 			if element (et_kicad.module_cursor).generic_name = module_B then
-- 				if element (et_kicad.module_cursor).instance = instance_B then
-- 					-- get the terminal count of connector B
-- 					terminal_count_B := et_kicad.terminal_count (reference_B, log_threshold + 2);
-- 					log ("module " & to_string (submodule => module_B) & " instance " 
-- 						& et_coordinates.to_string (instance_B) & " connector " 
-- 						& to_string (reference_B) & to_string (terminal_count_B),
-- 						log_threshold + 1);
-- 					module_found := true;
-- 					exit;
-- 				end if;
-- 			end if;
-- 			next (et_kicad.module_cursor);
-- 		end loop;
-- 
-- 		if not module_found then -- safety measure in case the module could not be found. should never happen
-- 			module_not_found (module_B, instance_B);
-- 		end if;
-- 
-- 
-- 
-- 		
-- 		-- if terminal counts differ, abort
-- 		if terminal_count_A /= terminal_count_B then
-- 			log_indentation_reset;
-- 			log (message_error 
-- 				& " module " & to_string (submodule => module_A) 
-- 				& " instance " & et_coordinates.to_string (instance_A)
-- 				& " connector " & to_string (reference_A)
-- 				& " and module " & to_string (submodule => module_B)
-- 				& " instance " & et_coordinates.to_string (instance_B)
-- 				& " connector " & to_string (reference_B)
-- 				& " do not match !",
-- 				console => true);
-- 			raise constraint_error;
-- 		end if;
-- 				 
-- 		log_indentation_down;
-- 	end compare_connector_terminal_count;


-- 	procedure compare_nets (
-- 	-- CS This procedure is for kicad only.
-- 	-- Compares net names of the given connectors (via kicad.module.netlist).
-- 	-- The net names, the ports and the terminal names on both sides of
-- 	-- the board-to-board connection must be equal.
-- 
-- 	-- The workflow in general:
-- 	-- 1. The comparing is conducted first from the right to the left. Means
-- 	-- module_A and reference_A are assumed to be on the right of the connection
-- 	-- while module_B and reference_B are assumed on the left.
-- 	-- 2. In module_right all nets having reference_right are located. Each occurence
-- 	-- stands for the port and terminal (pin/pad) of the connector on the right.
-- 	-- This is based on the netlist of the module (see type_module)
-- 	-- 3. For each port on the right the connector on the left is probed. The port on the left
-- 	-- must be connected to a net with the same name as the one on the right. 
-- 	-- Otherwise a warning is issued or error raised (parameter warn_only).
-- 	-- The left side connector is reference_left which must have the same port and terminal
-- 	-- name. Otherwise a warning is issued or error raised (parameter warn_only). Since the 
-- 	-- terminal names are not in the netlist, they are fetched via the connector reference
-- 	-- by function to_terminal.
-- 	-- 4. The modules A and B swap places. Means module_A and reference_A are assumed to be 
-- 	-- on the LEFT of the connection while module_B and reference_B are assumed on the RIGHT.
-- 	-- Why ? This way open ports are detected.
-- 	-- 5. Steps 2 and 3 are repeated.
-- 	
-- 	-- CS: There could be a time saving approach via the portlists of the modules. The connectors
-- 	-- ports and terminals could be tested for connected nets and compared ...
-- 	
-- 		module_A		: in et_coordinates.type_submodule_name.bounded_string;	-- nucleo_core
-- 		instance_A		: in et_coordinates.type_submodule_instance;			-- 1
-- 		reference_A		: in et_libraries.type_name;				-- X1
-- 		module_B		: in et_coordinates.type_submodule_name.bounded_string;	-- motor_driver
-- 		instance_B		: in et_coordinates.type_submodule_instance;			-- 4
-- 		reference_B		: in et_libraries.type_name;				-- X701
-- 		warn_only		: in type_net_comparator_warn_only;						-- warn or abort on difference
-- 		log_threshold	: in type_log_level) is
-- 
-- 		use et_coordinates;
-- 		--use et_schematic;
-- 		use et_libraries;
-- 		use et_kicad.type_rig;
-- 
-- 		module_cursor_right, module_cursor_left : et_kicad.type_rig.cursor;
-- 		net_right, net_left : et_schematic.type_net_name.bounded_string;	-- motor_on_off
-- 		port_right, port_left : et_kicad.type_port_with_reference;	-- 4
-- 		terminal_right, terminal_left : type_terminal;		-- 4, B3
-- 	
-- 		module_right : type_submodule_name.bounded_string := module_A;	-- nucleo_core
-- 		module_left : type_submodule_name.bounded_string := module_B;	-- motor_driver
-- 		module_swap : type_submodule_name.bounded_string;
-- 
-- 		instance_right : type_submodule_instance := instance_A; -- 1
-- 		instance_left : type_submodule_instance := instance_B; -- 4
-- 		instance_swap : type_submodule_instance;
-- 	
-- 		reference_right : type_name := reference_A;	-- X1
-- 		reference_left : type_name := reference_B;	-- X701
-- 		reference_swap : type_name;
-- 	
-- 		procedure query_nets_left (
-- 			module_name : in type_submodule_name.bounded_string;
-- 			module		: in et_kicad.type_module) is
-- 			use et_kicad.type_netlist;
-- 			net_cursor : et_kicad.type_netlist.cursor := module.netlist.first;
-- 
-- 			use et_schematic.type_net_name;
-- 			net_found : boolean := false;
-- 
-- 			function net_or_terminal_not_found return string is
-- 			begin
-- 				return "module " & to_string (module_left) 
-- 					& " : expect net " & et_schematic.to_string (net_name => net_right)
-- 					& " connected with " & to_string (reference_left)
-- 					& to_string (terminal_right) & "!";
-- 			end net_or_terminal_not_found;
-- 			
-- 			procedure query_ports_left (
-- 				net_name	: in et_schematic.type_net_name.bounded_string;
-- 				ports		: in et_kicad.type_ports_with_reference.set) is
-- 				use et_kicad.type_ports_with_reference;
-- 				use type_port_name;
-- 				port_cursor : et_kicad.type_ports_with_reference.cursor := ports.first;
-- 				terminal_found : boolean := false;
-- 			begin -- query_ports_left
-- 				log_indentation_up;
-- 				log ("locating connector " & to_string (reference => reference_left) 
-- 					& to_string (terminal_right) & "...", log_threshold + 8);
-- 
-- 				log_indentation_up;
-- 				while port_cursor /= et_kicad.type_ports_with_reference.no_element loop
-- 					port_left := element (port_cursor);
-- 
-- 					if port_left.reference = reference_left then
-- 						log ("connector found", log_threshold + 9);
-- 						
-- 						if port_left.name = port_right.name then
-- 							log ("port found", log_threshold + 9);
-- 
-- 							-- fetch terminal name from port_left and current module
-- 							terminal_left := et_kicad.to_terminal (port_left, module_name, log_threshold + 10);
-- 
-- 							-- compare terminal names. on match exit loop.
-- 							if terminal_left = terminal_right then
-- 								log ("terminal found", log_threshold + 9);
-- 								terminal_found := true;
-- 								exit;
-- 							end if;
-- 								
-- 						end if;
-- 					end if;
-- 					next (port_cursor);
-- 				end loop;
-- 				log_indentation_down;
-- 
-- 				-- If the expected terminal could not be found, issue warning or abort as specified
-- 				-- by input parameter warn_only.
-- 				if not terminal_found then
-- 					case warn_only is
-- 						when ON		=> 	log (message_warning & net_or_terminal_not_found); 
-- 						when OFF 	=>	
-- 							log_indentation_reset;
-- 							log (message_error & net_or_terminal_not_found, console => true); 
-- 							raise constraint_error;
-- 					end case;
-- 				end if;
-- 								  
-- 				log_indentation_down;
-- 			end query_ports_left;
-- 
-- 		begin -- query_nets_left
-- 			log ("locating net " & et_schematic.to_string (net_name => net_right) 
-- 				& " in module " & to_string (module_left) & " ...", log_threshold + 6);
-- 			log_indentation_up;
-- 
-- 			while net_cursor /= et_kicad.type_netlist.no_element loop
-- 				net_left := key (net_cursor);
-- 				log (et_schematic.to_string (net_name => net_left), log_threshold + 7);
-- 
-- 				if net_left = net_right then
-- 					net_found := true;
-- 					query_element (
-- 						position	=> net_cursor,
-- 						process		=> query_ports_left'access);
-- 					exit;
-- 				end if;
-- 
-- 				next (net_cursor);
-- 			end loop;
-- 
-- 			-- If expected net not found, issue warning or abort as specified by
-- 			-- input parameter warn_only.
-- 			if not net_found then
-- 				case warn_only is
-- 					when ON => log (message_warning & net_or_terminal_not_found); 
-- 					when OFF =>
-- 						log_indentation_reset;
-- 						log (message_error & net_or_terminal_not_found,
-- 							console => true); 
-- 						raise constraint_error;
-- 				end case;
-- 			end if;
-- 
-- 			log_indentation_down;
-- 		end query_nets_left;
-- 		
-- 		procedure query_nets_right (
-- 			module_name : in type_submodule_name.bounded_string;
-- 			module		: in et_kicad.type_module) is
-- 			use et_kicad.type_netlist;
-- 			net_cursor : et_kicad.type_netlist.cursor := module.netlist.first;
-- 
-- 			procedure query_ports_right (
-- 				net_name	: in et_schematic.type_net_name.bounded_string;
-- 				ports		: in et_kicad.type_ports_with_reference.set) is
-- 				use et_kicad.type_ports_with_reference;
-- 				port_cursor : et_kicad.type_ports_with_reference.cursor := ports.first;
-- 			begin -- query_ports_right
-- 				net_right := net_name;
-- 				log (et_schematic.to_string (net_right), log_threshold + 3);
-- 
-- 				log_indentation_up;				
-- 				log ("querying connector terminals ...", log_threshold + 4);
-- 				log_indentation_up;
-- 				
-- 				-- search for ports that have reference_right
-- 				while port_cursor /= et_kicad.type_ports_with_reference.no_element loop
-- 					if element (port_cursor).reference = reference_right then
-- 						port_right := element (port_cursor);
-- 
-- 						-- fetch terminal of port_right
-- 						terminal_right := et_kicad.to_terminal (port_right, module_name, log_threshold + 6);
-- 						
-- 						log (to_string (reference_right)
-- 							& to_string (terminal_right), log_threshold + 5);
-- 						
-- 						log_indentation_up;
-- 						
-- 						-- look up nets in module left
-- 						query_element (
-- 							position	=> module_cursor_left,
-- 							process		=> query_nets_left'access);
-- 
-- 						log_indentation_down;
-- 					end if;					
-- 					next (port_cursor);
-- 				end loop;
-- 								  
-- 				log_indentation_down;
-- 				log_indentation_down;
-- 			end query_ports_right;
-- 			
-- 		begin -- query_nets_right
-- 			log ("querying nets in module " & to_string (module_right) & " ...", log_threshold + 2);
-- 			log_indentation_up;
-- 
-- 			while net_cursor /= et_kicad.type_netlist.no_element loop
-- 
-- 				query_element (
-- 					position	=> net_cursor,
-- 					process		=> query_ports_right'access);
-- 
-- 				next (net_cursor);
-- 			end loop;
-- 
-- 			log_indentation_down;			
-- 		end query_nets_right;
-- 
-- 		
-- 		procedure set_module_cursors is
-- 			use type_submodule_name;
-- 		begin
-- 			log ("module right " & to_string (submodule => module_right) 
-- 				& " instance " & et_coordinates.to_string (instance_right)
-- 				& " connector right " & to_string (reference_right), log_threshold + 1);
-- 
-- 			log ("module left " & to_string (submodule => module_left) 
-- 				& " instance " & et_coordinates.to_string (instance_left)
-- 				& " connector left " & to_string (reference_left), log_threshold + 1);
-- 
-- 			-- locate the module in the rig by its generic name and instance
-- 			module_cursor_right := et_kicad.rig.first;
-- 			while module_cursor_right /= no_element loop
-- 				if element (module_cursor_right).generic_name = module_right then
-- 					if element (et_kicad.module_cursor).instance = instance_right then
-- 						exit;
-- 					end if;
-- 				end if;
-- 				next (module_cursor_right);
-- 			end loop;
-- 			-- now module_cursor_right should point to module right
-- 			-- CS: abort if module not found
-- 
-- 			-- locate the module in the rig by its generic name and instance			
-- 			module_cursor_left := et_kicad.rig.first;
-- 			while module_cursor_left /= no_element loop
-- 				if element (module_cursor_left).generic_name = module_left then
-- 					if element (et_kicad.module_cursor).instance = instance_left then
-- 						exit;
-- 					end if;
-- 				end if;
-- 				next (module_cursor_left);
-- 			end loop;
-- 			-- now module_cursor_left should point to module right
-- 			-- CS: abort gracefully if module not found
-- 
-- 			log_indentation_up;
-- 			query_element (
-- 				position	=> module_cursor_right,
-- 				process		=> query_nets_right'access);
-- 			log_indentation_down;
-- 
-- 		end set_module_cursors;
-- 
-- 		
-- 	begin -- compare_nets
-- 		log ("comparing net names ...", log_threshold);
-- 		log_indentation_up;
-- 
-- 		-- Test connection from right to left.
-- 		set_module_cursors;
-- 		
-- 		-- Test connection from left to right
-- 		-- swap places
-- 		module_swap := module_left; -- backup name of module left
-- 		module_left := module_right; -- left becomes right
-- 		module_right := module_swap; -- right becomes left
-- 
-- 		instance_swap := instance_left; -- backup instance on the left
-- 		instance_left := instance_right; -- left becomes right
-- 		instance_right := instance_swap; -- right becomes left
-- 		
-- 		reference_swap := reference_left; -- backup name of component reference left
-- 		reference_left := reference_right; -- left becomes right
-- 		reference_right := reference_swap; -- right becomes left
-- 
-- 		set_module_cursors;
-- 
-- 		log_indentation_down;
-- 	end compare_nets;
	
-- 	procedure validate_module_interconnections (log_threshold: in et_string_processing.type_log_level) is
-- 	-- Tests if module interconnections like "LMX 1 "PWR CTRL IN" PWR 1 "PWR CTRL OUT"" 
-- 	-- make sense at net level.
-- 	-- NOTE: call AFTER modules have been imported !
-- 		use type_module_interconnections;
-- 		use et_coordinates;
-- 		use et_libraries;
-- 		interconnection_cursor		: type_module_interconnections.cursor := module_interconnections.first;
-- 		module_A, module_B			: type_import_module;
-- 		instance_A, instance_B		: type_submodule_instance;
-- 		purpose_A, purpose_B 		: et_libraries.type_component_purpose.bounded_string;
-- 		reference_A, reference_B	: et_libraries.type_name;
-- 	begin
-- 		log ("validating module interconnections ...", log_threshold);
-- 		log_indentation_up;
-- 
-- 		-- From the generic module name (led_matrix), the module instance (1) and the 
-- 		-- purpose (PWR CTRL IN) of the connector we reason the references like X46.
-- 		-- This must be done on both sides of the interconnection (A and B)
-- 		while interconnection_cursor /= no_element loop
-- 
-- 			-- PEER A
-- 			
-- 			-- A: map from abbrevation to import module (like led_matrix LMX kicad_v4 2) :
-- 			module_A := to_submodule (element (interconnection_cursor).peer_A.abbrevation); -- LMX to led_matrix
-- 			log ("generic module A " & to_string (module_A.name), log_threshold + 2); -- led_matrix
-- 
-- 			instance_A := element (interconnection_cursor).peer_A.instance; -- 2
-- 			log ("instance A " & et_coordinates.to_string (instance_A), log_threshold + 2);
-- 			
-- 			-- A: map from module name and purpose to reference
-- 			purpose_A := element (interconnection_cursor).peer_A.purpose;
-- 			log ("purpose connector A " & enclose_in_quotes (to_string (purpose_A)), log_threshold + 2);
-- 
-- 			reference_A := to_connector_reference (
-- 				generic_module_name	=> module_A.name,	-- led_matrix
-- 				instance 			=> instance_A,		-- 1
-- 				purpose				=> purpose_A,		-- "PWR CTRL IN"
-- 				log_threshold => log_threshold + 3);
-- 			
-- 			log ("reference connector A " & to_string (reference_A), log_threshold + 2);
-- 
-- 
-- 			-- PEER B
-- 			
-- 			-- B: map from abbrevation to to import module (like pwr_supply PWR kicad_v4 1) :
-- 			module_B := to_submodule (element (interconnection_cursor).peer_B.abbrevation);
-- 			log ("generic module B " & to_string (module_B.name), log_threshold + 2);
-- 
-- 			instance_B := element (interconnection_cursor).peer_B.instance;
-- 			log ("instance B " & et_coordinates.to_string (instance_B), log_threshold + 2);
-- 			
-- 			-- B: map from module name and purpose to reference
-- 			purpose_B := element (interconnection_cursor).peer_B.purpose;
-- 			log ("purpose connector B " & enclose_in_quotes (to_string (purpose_B)), log_threshold + 2);
-- 			
-- 			reference_B := to_connector_reference (
-- 				generic_module_name	=> module_B.name,
-- 				instance			=> instance_B,
-- 				purpose				=> purpose_B,
-- 				log_threshold		=> log_threshold + 3);
-- 
-- 			log ("reference connector B " & to_string (reference_B), log_threshold + 2);
-- 
-- 
-- 			
-- 
-- 			-- compare connector terminal counts. each peer must have the same number of terminals
-- 			compare_connector_terminal_count (
-- 				module_A.name, instance_A, reference_A, -- led_matrix, 1, X46
-- 				module_B.name, instance_B, reference_B, -- pwr_supply, 1, X701
-- 				log_threshold + 1);
-- 			
-- 			-- compare net names
-- 			-- If net name comparator is turned off for this connection, this step is skipped.
-- 			-- Otherwise net names are compared on both sides of the module interconnection.
-- 			case element (interconnection_cursor).options.comparator is
-- 				when ON =>
-- 					compare_nets (
-- 						module_A		=> module_A.name,	-- led_matrix
-- 						instance_A		=> instance_A,		-- 1
-- 						reference_A		=> reference_A, 	-- X46
-- 						module_B 		=> module_B.name,	-- motor_driver
-- 						instance_B		=> instance_B,		-- 1
-- 						reference_B 	=> reference_B,		-- X701
-- 						warn_only		=> element (interconnection_cursor).options.warn_only, -- warnings or abort on difference
-- 						log_threshold 	=> log_threshold + 1);
-- 
-- 				when OFF =>
-- 					log ("net comparator off -> name comparing skipped !", log_threshold + 2);
-- 			end case;
-- 					
-- 			next (interconnection_cursor);
-- 		end loop;
-- 
-- 		log_indentation_down;
-- 	end validate_module_interconnections;
	
	function to_string (cat : in type_device_category) return string is
	-- returns the given component category as string
	begin
		return " " & type_device_category'image (cat);
	end to_string;

	function to_category (category : in string) return type_device_category is
	-- Converts a string to type_device_category.
		use et_string_processing;
		category_out : type_device_category;
	begin
		category_out := type_device_category'value (category);
		return category_out;

		exception
			when others =>
				log (ERROR, category & " is not a supported device category !",
					 console => true);

				log (text => "supported categories are:");
				for cat in type_device_category'pos (type_device_category'first) .. 
					type_device_category'pos (type_device_category'last) loop

					log (text => "- " & to_string (type_device_category'val (cat)));
				end loop;
				
				raise constraint_error;
	end to_category;

	function component_prefixes_specified return boolean is
	-- Returns true if any component prefixes are specified via conventions file.
		use type_component_prefixes;
	begin
		if is_empty (component_prefixes) then -- no prefixes specified
			return false;
		else -- prefixes are specified
			return true;
		end if;
	end component_prefixes_specified;
	
	function category (prefix : in et_devices.type_prefix.bounded_string) return
		type_device_category is
		use et_devices;
		use et_devices.type_prefix;
		use type_component_prefixes;

		prefix_cursor : type_component_prefixes.cursor;
	begin
		-- locate prefix as specified by conventions file
		prefix_cursor := component_prefixes.find (prefix);

		-- If prefix not specified (or no conventions at all) return category UNKNOWN.
		-- Otherwise return the respecitve category.
		if prefix_cursor = type_component_prefixes.no_element then
			log (WARNING, "category of prefix " 
				 & et_devices.to_string (prefix)
				 & latin_1.space
				 & to_string (UNKNOWN) & " !");
			return UNKNOWN;
		else
			return element (prefix_cursor);
		end if;
	end category;

	
	function category (reference : in type_name) return
		type_device_category is
	-- Returns the category of the given component reference. If no category could be
	-- found, returns category UNKNOWN.
		use type_prefix;
		use type_component_prefixes;

		prefix_cursor : type_component_prefixes.cursor;
	begin
		-- locate prefix as specified by conventions file
		prefix_cursor := component_prefixes.find (reference.prefix);

		-- If prefix not specified (or no conventions at all) return category UNKNOWN.
		-- Otherwise return the respecitve category.
		if prefix_cursor = type_component_prefixes.no_element then
			log (WARNING, " category of device " 
				 & to_string (reference)
				 & latin_1.space & to_string (UNKNOWN) & " !");
			return UNKNOWN;
		else
			return element (prefix_cursor);
		end if;
	end category;


-- 	function ports_in_net (
-- 		module 			: in et_coordinates.type_submodule_name.bounded_string;	-- led_matrix_2
-- 		net				: in et_schematic.type_net_name.bounded_string;			-- motor_on_off
-- 		category		: in type_device_category;				-- netchanger, connector
-- 		log_threshold	: in et_string_processing.type_log_level)
-- 		return et_kicad.type_ports_with_reference.set is
-- 	-- Returns a set of component ports that are connected with the given net.
-- 	-- Returns only components of given category.
-- 	
-- 	-- CS this function is for kicad only.
-- 
-- 		use et_libraries;
-- 		--use et_schematic;
-- 		use et_coordinates;
-- 		use et_string_processing;
-- 		use et_kicad.type_rig;
-- 		use et_kicad.type_ports_with_reference;
-- 
-- 		module_cursor : et_kicad.type_rig.cursor;
-- 		
-- 		ports_all 			: et_kicad.type_ports_with_reference.set;	-- all ports of the net
-- 		ports_by_category	: et_kicad.type_ports_with_reference.set; -- to be returned
-- 		port_cursor			: et_kicad.type_ports_with_reference.cursor;
-- 		port_scratch		: et_kicad.type_port_with_reference;
-- 		terminal 			: type_terminal;
-- 
-- 	begin -- ports_in_net
-- -- 		log ("locating" & to_string (category) & " ports in module " 
-- -- 			 & to_string (module) & " net " & to_string (net) & " ...",
-- -- 			 log_threshold);
-- 		log ("locating" & to_string (category) & " ports ...", log_threshold);
-- 		
-- 		log_indentation_up;
-- 
-- 		-- Get all the component ports in the net.
-- 		ports_all := et_kicad.components_in_net (
-- 						module			=> module,	-- led_matrix_2
-- 						net				=> net,
-- 						log_threshold	=> log_threshold + 2);
-- 
-- 		-- If there are ports in the given net, set port cursor to first port in net,
-- 		-- filter ports by appearance, category and log ports one after another.
-- 		-- If no ports in net, issue a warning.
-- 		if not is_empty (ports_all) then
-- 			port_cursor := ports_all.first;
-- 			while port_cursor /= et_kicad.type_ports_with_reference.no_element loop
-- 				port_scratch := element (port_cursor); -- load the port
-- 
-- 				-- only real components matter here:
-- 				if port_scratch.appearance = sch_pcb then
-- 					
-- 					-- filter by given category and insert the current port_scratch in ports_by_category 
-- 					if et_configuration.category (port_scratch.reference) = category then
-- 						terminal := et_kicad.to_terminal (port_scratch, module, log_threshold + 3); -- fetch the terminal
-- 						--log (to_string (port_scratch) & to_string (terminal, show_unit => true, preamble => true),
-- 						log (et_kicad.to_string (port_scratch) 
-- 							& " terminal " & to_string (terminal.name), --, show_unit => true, preamble => true),
-- 							log_threshold + 1);
-- 
-- 						-- insert in container (to be returned)
-- 						insert (
-- 							container	=> ports_by_category,
-- 							new_item	=> port_scratch);
-- 					end if;
-- 				end if;
-- 				
-- 				next (port_cursor);
-- 			end loop;
-- 		else
-- 			log (message_warning & "net " & et_schematic.to_string (net) & " is not connected with any ports !");
-- 		end if;
-- 
-- 		-- show number of component ports that have been found by given category.
-- 		log (" found" & count_type'image (ports_by_category.length) & " ports", log_threshold + 1);
-- 		
-- 		log_indentation_down;
-- 		return ports_by_category;
-- 	end ports_in_net;

-- 	function to_string (
-- 		net			: in type_net;
-- 		separator	: in character := '.') return string is
-- 		-- Returns the given net as string. In a form like "led_matrix.master_clock"
-- 
-- 		use et_coordinates.type_submodule_name;
-- 		use et_schematic.type_net_name;
-- 	begin
-- 		if length (net.module) = 0 or length (net.net) = 0 then
-- 			return "";
-- 		else
-- 			return to_string (net.module) & separator & to_string (net.net);
-- 		end if;
-- 	end to_string;
	

-- 	function is_module_interconnector (
-- 	-- Looks up the module_interconnections as specified in configuration file
-- 	-- in section MODULE_INTERCONNECTIONS.
-- 	-- Returns true if the given reference in given module is part of any module interconnection.
-- 	-- NOTE: The given module name is the GENERIC name of the module.
-- 		module			: in et_coordinates.type_submodule_name.bounded_string;	-- nucleo_core, led_matrix
-- 		instance		: in et_coordinates.type_submodule_instance;			-- 2
-- 		reference		: in et_libraries.type_name;				-- X701
-- 		log_threshold	: in et_string_processing.type_log_level 
-- 		) return boolean is
-- 		
-- 		use et_libraries;
-- 		use et_coordinates.type_submodule_name;
-- 		use type_module_interconnections;
-- 
-- 		connection_cursor : type_module_interconnections.cursor;
-- 		connection : type_module_interconnection;
-- 		result : boolean := false; -- to be returned
-- 	begin -- is_module_interconnector
-- -- 		log ("testing whether connector " & to_string (reference) 
-- -- 			 & " in generic module " & to_string (module) & " instance " & et_coordinates.to_string (instance)
-- -- 			 & " is a module connector ...", log_threshold);
-- 		log ("testing whether " & to_string (reference) & " is a module connector ...", log_threshold);
-- 
-- 		log_indentation_up;
-- 		
-- 		-- If there are module interconnections (specified in configuration file section MODULE_INTERCONNECTIONS)
-- 		-- locate the interconnections that address the given module. Other modules are not of interest.
-- 		-- If no module interconnections declared at all, return false.
-- 		if not is_empty (module_interconnections) then
-- 			connection_cursor := module_interconnections.first;
-- 			while connection_cursor /= type_module_interconnections.no_element loop
-- 				connection := element (connection_cursor);
-- 				
-- 				-- probe interconnection at peer A. Test if the connector reference at peer A 
-- 				-- matches the the given reference. On match return true.
-- 				if to_submodule (connection.peer_A.abbrevation).name = module then
-- 					if connection.peer_A.instance = instance then
-- 
-- 						-- The connector reference (like X701) can be reasoned from
-- 						-- the generic module name and the instance.
-- 						if to_connector_reference (
-- 								generic_module_name	=> module,		-- led_matrix
-- 								instance 			=> instance,	-- 2
-- 								purpose				=> connection.peer_A.purpose, -- PWR_IN
-- 								log_threshold		=> log_threshold + 1) = reference then
-- 							result := true;
-- 							exit;
-- 						end if;
-- 					end if;
-- 				end if;
-- 
-- 				-- probe interconnection at peer B. Test if the connector reference at peer B
-- 				-- matches the the given reference. On match return true.				
-- 				if to_submodule (connection.peer_B.abbrevation).name = module then
-- 					if connection.peer_B.instance = instance then
-- 
-- 						-- The connector reference (like X701) can be reasoned from
-- 						-- the generic module name and the instance.
-- 						if to_connector_reference (
-- 								generic_module_name	=> module,
-- 								instance 			=> instance,
-- 								purpose				=> connection.peer_B.purpose,
-- 								log_threshold		=> log_threshold + 1) = reference then
-- 							result := true;
-- 							exit;
-- 						end if;
-- 					end if;
-- 				end if;
-- 				
-- 				next (connection_cursor);
-- 			end loop;
-- 		end if;
-- 
-- 		-- show result
-- 		case result is
-- 			when true => log ("yes", log_threshold);
-- 			when false => log ("no", log_threshold);
-- 		end case;
-- 		
-- 		log_indentation_down;
-- 		return result;
-- 	end is_module_interconnector;
	
-- 	function connectors_in_net (
-- 	-- Returns for the given net the ports which belong to a module interconnection.
-- 	-- If the net is not connected with any module interconnectors the returned list is empty.
-- 	-- If there are no module interactions declared at all, the returned list is empty.
-- 	-- This requires to look up the interconnections declared in the configuration file.
-- 	-- So the generic module name and the instance matter here.	
-- 		module 			: in et_coordinates.type_submodule_name.bounded_string;	-- led_matrix_2
-- 		generic_name 	: in et_coordinates.type_submodule_name.bounded_string; -- led_matrix
-- 		instance		: in et_coordinates.type_submodule_instance;			-- 2
-- 		net				: in et_schematic.type_net_name.bounded_string;			-- motor_on_off
-- 		log_threshold	: in et_string_processing.type_log_level)
-- 		return et_kicad.type_ports_with_reference.set is
-- 
-- 		use et_kicad;
-- 		use et_kicad.type_ports_with_reference;
-- 
-- 		ports_all : et_kicad.type_ports_with_reference.set;
-- 		ports_of_interconnection : et_kicad.type_ports_with_reference.set; -- to be returned
-- 		port_cursor : et_kicad.type_ports_with_reference.cursor;
-- 
-- 	begin -- connectors_in_net
-- 		--log ("locating ports of module interconnections in net " & to_string (net) & " ...", log_threshold);
-- 		log ("locating interconnection ports ...", log_threshold);
-- 		log_indentation_up;
-- 
-- 		-- Module interactions are made of connectors only. So we first load ALL connectors
-- 		-- connected with the given net:
-- 		ports_all := ports_in_net (module, net, CONNECTOR, log_threshold + 1);
-- 
-- 		-- If the net is not connected to any connectors, there is nothing to do. Otherwise
-- 		-- we start looping through ports_all and filter out those connectors which belong
-- 		-- to a module interconnection.
-- 		if not is_empty (ports_all) then
-- 			log_indentation_up;
-- 			
-- 			port_cursor := ports_all.first;
-- 			while port_cursor /= type_ports_with_reference.no_element loop
-- 
-- 				-- If port belongs to module interconnection, insert it in the list 
-- 				-- ports_of_interconnection (to be returned later).
-- 				if is_module_interconnector (
-- 						module			=> generic_name,					-- led_matrix
-- 						instance		=> instance,						-- 2
-- 						reference		=> element (port_cursor).reference,	-- X701
-- 						log_threshold	=> log_threshold + 2) then
-- 					
-- 					insert (ports_of_interconnection, element (port_cursor));
-- 				end if;
-- 				
-- 				next (port_cursor);
-- 			end loop;
-- 
-- 			log_indentation_down;
-- 		end if;
-- 
-- 		log_indentation_down;
-- 		return ports_of_interconnection;
-- 	end connectors_in_net;

-- 	function opposide_connector_port (
-- 	-- Returns the counterpart of the given connector port on the opposide of the module interconnection.
-- 		module_name		: in et_coordinates.type_submodule_name.bounded_string; -- led_matrix_2
-- 		port			: in et_kicad.type_port_with_reference;
-- 		log_threshold	: in et_string_processing.type_log_level)
-- 		return et_kicad.type_port_of_module is
-- 		opposide_port : et_kicad.type_port_of_module; -- to be returned
-- 
-- 		use et_libraries;
-- 		--use et_schematic;
-- 		use et_kicad.type_rig;
-- 		module_cursor : et_kicad.type_rig.cursor;
-- 		connector_found : boolean := false; -- goes true once the opposide connector has been found
-- 	
-- 		generic_module_name_opposide : et_coordinates.type_submodule_name.bounded_string; -- pwr_supply
-- 		reference_opposide : et_libraries.type_name; -- X45
-- 
-- 		use type_module_interconnections;
-- 		interconnection_cursor : type_module_interconnections.cursor := module_interconnections.first;
-- 		interconnection : type_module_interconnection; -- for temporarily storage of a module interconnection
-- 		connector : type_connector; -- temporarily storage of a connector
-- 	
-- 	begin -- opposide_connector_port
-- 		log ("locating connector port opposide of " & to_string (port.reference)
-- 			& " port " & to_string (port.name) & " ...", log_threshold);
-- 		log_indentation_up;
-- 		
-- 		-- set module cursor to the given module. CS it should be found, otherwise exception occurs.
-- 		module_cursor := find (et_kicad.rig, module_name);
-- 
-- 		-- BUILD GIVEN CONNECTOR 
-- 		log ("given module " & et_coordinates.to_string (module_name), log_threshold + 1);
-- 		
-- 		-- fetch abbrevation of module
-- 		connector.abbrevation := to_abbrevation (module_name);
-- 		log ("given module abbrevation " & et_coordinates.to_string (connector.abbrevation), log_threshold + 1);
-- 		
-- 		-- fetch module instance
-- 		connector.instance := element (module_cursor).instance;
-- 		log ("given module instance " & et_coordinates.to_string (connector.instance), log_threshold + 1);
-- 		
-- 		-- fetch purpose of component of given port
-- 		connector.purpose := et_kicad.purpose (module_name, port.reference, log_threshold + 1);
-- 		log ("given connector purpose " & to_string (connector.purpose), log_threshold + 1);
-- 
-- 		log ("given connector reference " & to_string (port.reference), log_threshold + 1);
-- 		log ("given connector port name " & to_string (port.name), log_threshold + 1);
-- 
-- 		
-- 		-- SEARCH GIVEN CONNECTOR
-- 		
-- 		-- Loop through module interconnections and test whether the connector is
-- 		-- peer A or B. On match variable "connector" assumes the connector properties
-- 		-- of the opposide.
-- 		while interconnection_cursor /= type_module_interconnections.no_element loop
-- 			interconnection := element (interconnection_cursor);
-- 
-- 			-- test connector at peer A. on match exit with connector at peer B
-- 			if interconnection.peer_A = connector then
-- 				connector := interconnection.peer_B;
-- 				connector_found := true;
-- 				exit;
-- 			end if;
-- 
-- 			-- test connector at peer B. on match exit with connector at peer A
-- 			if interconnection.peer_B = connector then
-- 				connector := interconnection.peer_A;
-- 				connector_found := true;
-- 				exit;
-- 			end if;
-- 			
-- 			next (interconnection_cursor);
-- 		end loop;
-- 
-- 		if not connector_found then
-- 			log_indentation_reset;
-- 			log (message_error & " in module " & et_coordinates.to_string (module_name) 
-- 				 & " abbrevation " & et_coordinates.to_string (connector.abbrevation)
-- 				 & " instance " & et_coordinates.to_string (connector.instance)
-- 				 & " no connector with purpose " & to_string (purpose)
-- 				 & " found !");
-- 			raise constraint_error;
-- 		end if;
-- 
-- 		-- BUILD OPPOSIDE CONNECTOR
-- 		log ("opposide module abbrevation "	& et_coordinates.to_string (connector.abbrevation), log_threshold + 1);		
-- 		log ("opposide module instance " 	& et_coordinates.to_string (connector.instance), log_threshold + 1);
-- 		log ("opposide connector purpose "	& to_string (connector.purpose), log_threshold + 1);
-- 		
-- 		-- fetch generic module name of opposide peer
-- 		generic_module_name_opposide := to_submodule (connector.abbrevation).name;
-- 		log ("opposide generic module " & et_coordinates.to_string (generic_module_name_opposide), log_threshold + 1);
-- 		
-- 		-- fetch connector reference on opposide 
-- 		reference_opposide := to_connector_reference ( -- x45
-- 			generic_module_name	=> generic_module_name_opposide,	-- pwr_supply
-- 			instance			=> connector.instance,				-- 1
-- 			purpose				=> connector.purpose,				-- PWR_OUT
-- 			log_threshold		=> log_threshold + 1);
-- 
-- 		-- If there is just a single instance of the opposide module declared (configuration file 
-- 		-- section [IMPORT_MODULES]) the generic module name to be returned. Thus no indexing like pwr_supply_2
-- 		-- but just pwr_supply.
-- 		-- If more than one instance declared the module instance is appended.
-- 		if to_submodule (connector.abbrevation).instances = et_coordinates.type_submodule_instance'first then
-- 			opposide_port.module := generic_module_name_opposide; -- single instance
-- 		else
-- 			-- The module name is composed of the generic module name and the instance.
-- 			opposide_port.module := et_coordinates.append_instance (	-- pwr_supply_1
-- 					submodule 	=> generic_module_name_opposide,		-- pwr_supply
-- 					instance	=> connector.instance);					-- 1
-- 		end if;
-- 		
-- 		log ("opposide module " & et_coordinates.to_string (opposide_port.module), log_threshold + 1);
-- 		
-- 		opposide_port.reference := reference_opposide; -- X45
-- 		log ("opposide connector reference " & to_string (opposide_port.reference), log_threshold + 1);
-- 
-- 		-- The port name is the same as the given port name.
-- 		opposide_port.name := port.name;
-- 		log ("opposide port " & to_string (opposide_port.name), log_threshold + 1);
-- 		
-- 		return opposide_port;
-- 	end opposide_connector_port;

-- 	function opposide_netchanger_port (
-- 	-- Returns the opposide port of the given netchanger port. If given port 1 returns port 2 and vice versa.
-- 		port : in et_libraries.type_port_name.bounded_string) return et_libraries.type_port_name.bounded_string is
-- 		use et_libraries;
-- 		use type_port_name;
-- 		name : type_port_name.bounded_string;
-- 	begin
-- 		if port = to_bounded_string ("1") then 
-- 			name := to_bounded_string ("2");
-- 		elsif port = to_bounded_string ("2") then
-- 			name := to_bounded_string ("1");
-- 		else
-- 			log_indentation_reset;
-- 			log (message_error & "components of category " & to_string (NETCHANGER) 
-- 				 & " must have port names like '1' or '2' !");
-- 			raise constraint_error;
-- 		end if;
-- 
-- 		return name;
-- 	end opposide_netchanger_port;

-- 	function compare_nets (left, right : in type_net) return boolean is
-- 	-- Returns true if left net comes before right net.
-- 		use et_coordinates.type_submodule_name;
-- 		use et_schematic.type_net_name;
-- 	begin
-- 		if left.module < right.module then
-- 			return true;
-- 		elsif left.module > right.module then
-- 			return false;
-- 		elsif left.net < right.net then
-- 			return true;
-- 		else 
-- 			return false;
-- 		end if;
-- 	end compare_nets;

-- 	function to_string (route_length : in type_route_length) return string is
-- 	-- Returns the given route length as string;
-- 	begin
-- 		return trim (type_route_length'image (route_length), left);
-- 	end to_string;
	
-- 	function longest_route (table : in type_routing_table.list) return type_route_length is
-- 	-- Returns the length of the longest route in the given routing table.
-- 	-- NOTE: assumes that the given routing table is not empty. Raises error othewise.
-- 		use type_routing_table;
-- 		use type_route;
-- 		route_cursor : type_routing_table.cursor := table.first;
-- 		route_length_scratch : type_route_length;
-- 		route_length : type_route_length := type_route_length'first; -- to be returned
-- 	begin
-- 		-- Loop in routes of given routing table.
-- 		while route_cursor /= type_routing_table.no_element loop
-- 
-- 			-- get length of current route
-- 			route_length_scratch := type_route_length (length (element (route_cursor)));
-- 
-- 			-- if current length is greater than previous length, 
-- 			-- update route_length. Otherwise route_length remains unchanged.
-- 			if route_length_scratch > route_length then
-- 				route_length := route_length_scratch;
-- 			end if;
-- 			
-- 			next (route_cursor);
-- 		end loop;
-- 
-- 		return route_length;
-- 	end longest_route;
-- 		
-- 	procedure make_routing_tables (log_threshold : in et_string_processing.type_log_level) is
-- 	-- Creates the routing table for the whole rig in global variable routin_table.
-- 	-- CS: create routing tables for projects separately.	
-- 		use et_string_processing;
-- 		use et_coordinates;
-- 		--use et_schematic;
-- 		use et_kicad.type_rig;
-- 
-- 		module_cursor : et_kicad.type_rig.cursor; -- points to the module being processed
-- 		route : type_route.set; -- for temporarily storage of a single route. 
-- 	
-- 		-- nets that have been processed are stored in a list of this type
-- 		package type_nets is new ordered_sets (element_type => type_net, "<" => compare_nets); use type_nets;
-- 		processed_nets : type_nets.set; -- finally stored here
-- 
-- 		procedure find_ports_by_net (
-- 		-- Insertes the net (if not already processed) in the current route.
-- 		-- Locates netchanger and connector ports in the given net.
-- 		-- Locates the nets connected with the netchangers and connectors and calls itself again.
-- 			module_name		: in et_coordinates.type_submodule_name.bounded_string;	-- the module to search in
-- 			net_name		: in et_schematic.type_net_name.bounded_string;			-- the net name
-- 			log_threshold	: in type_log_level) is
-- 
-- 			use et_libraries;
-- 			use et_schematic.type_net_name;
-- 			use et_kicad.type_ports_with_reference;
-- 
-- 			net_not_processed_yet : boolean; -- true if the given net has NOT been processed already
-- 			net_cursor : type_nets.cursor; -- no evaluation
-- 			
-- 			netchanger_port_opposide : type_port_name.bounded_string;
-- 			connector_port_opposide : et_kicad.type_port_of_module;
-- 			
-- 			net_name_opposide : et_schematic.type_net_name.bounded_string;
-- 			
-- 			netchangers	: et_kicad.type_ports_with_reference.set; -- the netchangers connected with the net
-- 			netchanger_cursor : et_kicad.type_ports_with_reference.cursor;
-- 			
-- 			connectors 	: et_kicad.type_ports_with_reference.set; -- the module interconnectors connected with the net
-- 			connector_cursor : et_kicad.type_ports_with_reference.cursor;
-- 
-- 			module_cursor : et_kicad.type_rig.cursor := find (et_kicad.rig, module_name);
-- 
-- 		begin -- find_ports_by_net
-- -- 			log ("locating ports in module " & et_coordinates.to_string (module_name) 
-- -- 				& " net " & et_schematic.to_string (net_name) & " ...", log_threshold);
-- -- 			log_indentation_up;
-- -- 			
-- 			insert (
-- 				container	=> processed_nets, 
-- 				new_item	=> (module => module_name, net => net_name),
-- 				position	=> net_cursor, -- no further evaluation
-- 				inserted	=> net_not_processed_yet);
-- 
-- 			-- If the net has been processed already we do nothing. Otherwise the 
-- 			-- net is inserted in the route being built currently.
-- 			-- Then ports of netchangers and module interconnections are located.
-- 			if net_not_processed_yet then
-- -- 				log ("locating ports in module " & et_coordinates.to_string (module_name) 
-- -- 					& " net " & et_schematic.to_string (net_name) & " ...", log_threshold);
-- -- 				log_indentation_up;
-- 
-- 				-- Insert net module and net name in current route.
-- 				type_route.insert (
-- 					container => route,
-- -- 					position => route_cursor,
-- 					new_item => (module => module_name, net => net_name));
-- 
-- 				log ("locating routing ports ...", log_threshold);
-- 				log_indentation_up;
-- 
-- 				-- NETCHANGERS
-- 
-- 				-- load all netchangers connected with this net
-- 				netchangers := ports_in_net (
-- 							module 			=> module_name, -- led_matrix_2
-- 							net				=> net_name,	-- motor_on_off
-- 							category		=> NETCHANGER,
-- 							log_threshold	=> log_threshold + 1);
-- 
-- 				if not is_empty (netchangers) then
-- 					log_indentation_up;
-- 					log ("locating nets connected with netchangers ...", log_threshold + 1);
-- 					log_indentation_up;
-- 
-- 					-- set cursor to first netchanger in this net
-- 					netchanger_cursor := netchangers.first;
-- 
-- 					-- loop in netchangers of this net
-- 					-- Get opposide port and net. The component reference on the opposide is the same.
-- 					while netchanger_cursor /= et_kicad.type_ports_with_reference.no_element loop
-- 						log (to_string (element (netchanger_cursor).reference) 
-- 							& " port " & to_string (element (netchanger_cursor).name),
-- 							log_threshold + 2);
-- 						log_indentation_up;
-- 
-- 						-- get opposide port
-- 						netchanger_port_opposide := opposide_netchanger_port (element (netchanger_cursor).name);
-- 
-- 						-- get opposide net
-- 						net_name_opposide := et_kicad.connected_net (
-- 												port => (
-- 													module 		=> module_name, -- module of origin like led_matrix_2
-- 													reference 	=> element (netchanger_cursor).reference, -- component of origin like N3
-- 													name 		=> netchanger_port_opposide), -- port name of origin like 1
-- 												log_threshold => log_threshold + 3);
-- 
-- 						-- If there is a net connected at the other side, find ports connected with this net.
-- 						-- If no net connected, we hava a dead end and issue a warning.
-- 						if length (net_name_opposide) > 0 then
-- 							log_indentation_up;
-- 							log ("connected with net " & et_schematic.to_string (net_name_opposide), log_threshold + 2);
-- 							log_indentation_up;
-- 							
-- 							-- locate ports of this net
-- 							find_ports_by_net (module_name, net_name_opposide, log_threshold + 3);
-- 							
-- 							log_indentation_down;
-- 							log_indentation_down;
-- 						else
-- 							-- dead end. netchanger port not connected
-- 							log (message_warning & " no net connected with " 
-- 								& to_string (element (netchanger_cursor).reference) 
-- 								& " port " & to_string (netchanger_port_opposide));
-- 						end if;
-- 						
-- 						next (netchanger_cursor);
-- 						log_indentation_down;
-- 					end loop;
-- 					
-- 					log_indentation_down;
-- 					log_indentation_down;
-- 				end if;
-- 
-- 				-- CONNECTORS
-- 
-- 				-- Load all connectors used for module interconnections connected with this net.
-- 				-- This requires to look up the interconnections declared in the configuration file.
-- 				-- So the generic module name and the instance matter here.
-- 				connectors := connectors_in_net (
-- 							module			=> module_name, 							-- led_matrix_2
-- 							generic_name 	=> element (module_cursor).generic_name,	-- led_matrix
-- 							instance 		=> element (module_cursor).instance,		-- 2
-- 							net				=> net_name,								-- motor_on_off
-- 							log_threshold	=> log_threshold + 1);
-- 
-- 				if not is_empty (connectors) then
-- 					log_indentation_up;
-- 					log ("locating nets connected with connectors ...", log_threshold + 1);
-- 					log_indentation_up;
-- 
-- 					-- set cursor to first connector in this net
-- 					connector_cursor := connectors.first;
-- 
-- 					-- loop in connectors of this net
-- 					-- Get opposide reference, port and net.
-- 					while connector_cursor /= et_kicad.type_ports_with_reference.no_element loop
-- 						log (to_string (element (connector_cursor).reference) 
-- 							& " port " & to_string (element (connector_cursor).name),
-- 							log_threshold + 2);
-- 						log_indentation_up;
-- 
-- 						-- get opposide port
-- 						connector_port_opposide := opposide_connector_port (
-- 													module_name => module_name, -- module of origin like led_matrix_2
-- 													port => element (connector_cursor), -- connector port of origin like X3 port 25
-- 													log_threshold => log_threshold + 3);
-- 
-- 						-- get opposide net
-- 						net_name_opposide := et_kicad.connected_net (
-- 												port => connector_port_opposide,
-- 													-- contains:
-- 													--  module to search in like pwr_supply_1
-- 													--  component to search for like X3
-- 													--  port name to search for like 25
-- 
-- 												log_threshold => log_threshold + 3);
-- 
-- 						-- If there is a net connected at the other side, find ports connected with this net.
-- 						-- If no net connected, we hava a dead end and issue a warning.
-- 						if length (net_name_opposide) > 0 then
-- 							log_indentation_up;
-- 							log ("connected with module " & to_string (connector_port_opposide.module) 
-- 								 & " net " & et_schematic.to_string (net_name_opposide), log_threshold + 2);
-- 							log_indentation_up;
-- 							
-- 							-- locate ports of this net
-- 							find_ports_by_net (connector_port_opposide.module, net_name_opposide, log_threshold + 3);
-- 							
-- 							log_indentation_down;
-- 							log_indentation_down;
-- 						else
-- 							-- dead end. netchanger port not connected
-- 							log (message_warning & " no net connected with module " & to_string (connector_port_opposide.module)
-- 								& " connector " & to_string (connector_port_opposide.reference) 
-- 								& " port " & to_string (connector_port_opposide.name));
-- 						end if;
-- 						
-- 						next (connector_cursor);
-- 						log_indentation_down;
-- 					end loop;
-- 					
-- 					log_indentation_down;
-- 					log_indentation_down;
-- 				end if;
-- 
-- 
-- 				
-- 				log_indentation_down;
-- 			else
-- 				log ("already processed -> skipped", log_threshold);
-- 			end if;
-- 
-- -- 			log_indentation_down;
-- 			
-- 			exception
-- 				when event:
-- 					others =>
-- 						log_indentation_reset;
-- 						put_line (ada.exceptions.exception_message (event));
-- 						raise;
-- 					
-- 		end find_ports_by_net;
-- 
-- 	
-- 		procedure query_nets (
-- 		-- Loops in netlist of current module. For each net the procedure find_ports_by_net
-- 		-- is called. find_ports_by_net calls itself over and over until all connected
-- 		-- nets have been located. A new route starts if the loop below calls find_ports_by_net.
-- 		-- Once find_ports_by_net finishes, the current route is complete. If more than one 
-- 		-- net is in the route, the route is appended to the global rig wide routing table.
-- 			module_name	: in type_submodule_name.bounded_string;	-- led_matrix_2
-- 			module		: in et_kicad.type_module) is
-- 			use et_kicad.type_netlist;
-- 			netlist		: et_kicad.type_netlist.map := module.netlist;
-- 			net_cursor	: et_kicad.type_netlist.cursor;
-- 			net_name	: et_schematic.type_net_name.bounded_string;
-- 
-- 			indentation_backup : type_indentation_level;
-- 			
-- 		begin -- query_nets
-- 			log ("querying nets ...", log_threshold + 2);
-- 			log_indentation_up;
-- 
-- 			-- backup the current log indentation
-- 			indentation_backup := log_indentation;
-- 			
-- 			if not is_empty (netlist) then
-- 				net_cursor := netlist.first;
-- 				while net_cursor /= et_kicad.type_netlist.no_element loop
-- 					net_name := key (net_cursor);
-- 
-- 					-- restore log indentation (to prevent log messages shifting infinitely to the right)
-- 					log_indentation := indentation_backup;
-- 					log (et_schematic.to_string (net_name), log_threshold + 3);
-- 
-- 					-- here a new route starts, clean up container "route" from previous spins:
-- 					type_route.clear (route);
-- 					
-- 					log_indentation_up;
-- 					find_ports_by_net (module_name, net_name, log_threshold + 4);
-- 					log_indentation_down;
-- 
-- 					-- Here the route ends. If more than one net collected in container "route"
-- 					-- we regard the collection as route -> append to routing table.
-- 					-- If there is only one net in the route it is discarded.
-- 					if type_route.length (route) > 1 then
-- 						type_routing_table.append (
-- 							container	=> routing_table,
-- 							new_item	=> route);
-- 					end if;
-- 					
-- 					next (net_cursor);
-- 				end loop;
-- 			else
-- 				log (message_warning & "module " & to_string (module_name) & " does not have any nets !");
-- 			end if;
-- 
-- 			log_indentation_down;
-- 		end query_nets;
-- 	
-- 	begin -- make_routing_tables
-- 		log ("making routing tables ...", log_threshold);
-- 		log_indentation_up;
-- 
-- 		module_cursor := et_kicad.rig.first;
-- 		
-- 		while module_cursor /= et_kicad.type_rig.no_element loop
-- 			log ("module " & to_string (key (module_cursor)), log_threshold + 1);
-- 			log_indentation_up;
-- 
-- 			query_element (
-- 				position	=> module_cursor,
-- 				process 	=> query_nets'access);
-- 			
-- 			log_indentation_down;
-- 			next (module_cursor);
-- 		end loop;
-- 		
-- 		log_indentation_down;
-- 	end make_routing_tables;

-- 	procedure export_routing_tables (log_threshold : in et_string_processing.type_log_level) is 
-- 	-- Exports/Writes the routing table of the rig in a csv file.
-- 	-- Reads the global rig wide routing table variable routing_table. 
-- 	-- Requires that procedure make_routing_tables has been executed before.
-- 	-- CS: Export routing tables for projects separately.
-- 	
-- 		use et_csv;
-- 
-- 		-- Get number of routes. This number determines the number of columns in the csv file.
-- 		routes_total : et_csv.type_column := et_csv.type_column (type_routing_table.length (routing_table));
-- 		longest_route : type_route_length; -- will later hold the greates length of a route
-- 
-- 		-- The min. number of columns in the csv file depends on max. number of fields required by file header.
-- 		-- NOTE: Adapt this constant when changing the csv file header.
-- 		columns_min : constant et_csv.type_column := 3; 
-- 
-- 		-- This variable assumes the number of columns required for the csv file.
-- 		columns_total : et_csv.type_column;
-- 
-- 		routing_handle : ada.text_io.file_type; -- the csv file handle
-- 	
-- 		function file_routing_table return string is
-- 		-- Returns the relative path and name of the routing table csv file.
-- 			use et_general;
-- 		begin
-- 			return compose ( 
-- 				containing_directory => compose (work_directory, report_directory),
-- 				name => "routing_table",
-- 				extension => et_csv.file_extension
-- 				);
-- 		end file_routing_table;
-- 
-- 		procedure create_routing_table_header is
-- 		-- Creates the routing table csv file in report_directory.
-- 		-- Writes some statistical information.
-- 		-- Leaves the file open for further puts.
-- 		begin
-- 			-- create the file. overwrites the previous one.
-- 			create (file => routing_handle, mode => out_file, name => file_routing_table);
-- 
-- 			-- write file header
-- 			reset_column;
-- 			put_field (file => routing_handle, text => et_general.system_name);
-- 			put_field (file => routing_handle, text => et_general.version);
-- 			put_field (file => routing_handle, text => "routing table");
-- 			put_lf (file => routing_handle, field_count => columns_total);
-- 
-- 			-- CS rig name. mind columns_min. see note above.
-- 
-- 			-- write date
-- 			put_field (file => routing_handle, text => "date");
-- 			put_field (file => routing_handle, text => (date (preamble => false)));
-- 			put_lf (file => routing_handle, field_count => columns_total);
-- 			
-- 			-- number of routes
-- 			put_field (file => routing_handle, text => "routes total");
-- 			put_field (file => routing_handle, text => to_string (routes_total));
-- 			put_lf (file => routing_handle, field_count => columns_total);
-- 
-- 			-- longest route
-- 			put_field (file => routing_handle, text => "greatest length");
-- 			put_field (file => routing_handle, text => to_string (longest_route));
-- 			put_lf (file => routing_handle, field_count => columns_total);
-- 
-- 			-- row separator
-- 			put_field (file => routing_handle, text => et_csv.row_separator_1);
-- 			put_lf (file => routing_handle, field_count => columns_total);
-- 			
-- 		end create_routing_table_header;
-- 
-- 		-- For exporting the routing_table in a csv file, this intermediate array type is 
-- 		-- required. For the moment it is unconstrained. The columns will later hold the
-- 		-- routes, while the rows hold the nets.
-- 		type type_routing_matrix is array (
-- 			positive range <>,	-- columns (or the width)
-- 			positive range <>) 	-- rows (or the length)
-- 			of type_net;
-- 		
-- 		function create_routing_matrix return type_routing_matrix is
-- 		-- By the total number of routes and the longest route among them the routing
-- 		-- matrix can be constrained and filled by the content of the routing_table.
-- 		-- The routing matrix is returned finally.
-- 			first_column	: constant positive := positive'first;
-- 			first_row		: constant positive := positive'first;
-- 
-- 			column	: positive := first_column;
-- 			row		: positive := first_row;
-- 
-- 			procedure increment_column is begin column := column + 1; end increment_column;
-- 			procedure reset_column is begin column := first_column; end reset_column;
-- 			procedure increment_row is begin row := row + 1; end increment_row;
-- 			procedure reset_row is begin row := first_row; end reset_row;
-- 
-- 			-- constrain the routing matrix
-- 			subtype type_routing_matrix_sized is type_routing_matrix (
-- 				first_column .. positive (routes_total), 	-- columns/width
-- 				first_row    .. positive (longest_route));	-- rowss/length
-- 
-- 			-- this is the routing matrix finally
-- 			routing_matrix : type_routing_matrix_sized;
-- 
-- 			-- set route cursor to first route in routing_table
-- 			use type_routing_table;
-- 			route_cursor : type_routing_table.cursor := routing_table.first;
-- 
-- 			-- for temporarily storage of a single route:
-- 			route : type_route.set;
-- 			
-- 			use type_route;
-- 			net_cursor : type_route.cursor;	-- points to a single net of a route
-- 
-- 			-- for temporarily storage of a single net
-- 			net : type_net;
-- 			
-- 		begin -- create_routing_matrix
-- 			log ("routes total " & to_string (routes_total), log_threshold + 2);
-- 			log ("greatest length " & to_string (longest_route), log_threshold + 2);
-- 
-- 			-- Loop in routes and nets and fill routing_matrix.
-- 			reset_column;
-- 			while route_cursor /= type_routing_table.no_element loop
-- 
-- 				-- fetch a route from the routing_table
-- 				route := element (route_cursor);
-- 
-- 				-- set cursor to first net of route
-- 				net_cursor := route.first;
-- 				reset_row; -- we start with the topmost row
-- 
-- 				-- loop in nets of current route
-- 				while net_cursor /= type_route.no_element loop
-- 
-- 					-- Fetch a net from the current route and write
-- 					-- it in the routing_matrix:
-- 					net := element (net_cursor);
-- 					routing_matrix (column,row) := net;
-- 
-- 					increment_row;
-- 					next (net_cursor);
-- 				end loop;
-- 
-- 				increment_column;
-- 				next (route_cursor);
-- 			end loop;
-- 
-- 			return routing_matrix;
-- 		end create_routing_matrix;
-- 		
-- 		procedure write_routes (routing_matrix : in type_routing_matrix) is
-- 		-- Reads the given routing_matrix and dumps its content in the csv file.
-- 		begin
-- 			log ("matrix rows" & positive'image (routing_matrix'first (2)) & " .."
-- 				& positive'image (routing_matrix'last (2)), log_threshold + 3);
-- 			log ("matrix cols" & positive'image (routing_matrix'first (1)) & " .."
-- 				& positive'image (routing_matrix'last (1)), log_threshold + 3);
-- 
-- 			-- write the column header: route #1, route #2, route #3 ...
-- 			reset_column;
-- 			for column in routing_matrix'first (1) .. routing_matrix'last (1) loop 
-- 				put_field (file => routing_handle, text => "route #" & positive'image (column));
-- 			end loop;
-- 			put_lf (file => routing_handle, field_count => routes_total);
-- 			put_lf (file => routing_handle, field_count => routes_total); -- empty line
-- 			
-- 			-- loop in rows which are the nets
-- 			for row in routing_matrix'first (2) .. routing_matrix'last (2) loop 
-- 
-- 				-- loop in columns which are the routes
-- 				reset_column;
-- 				for column in routing_matrix'first (1) .. routing_matrix'last (1) loop 
-- 					put_field (
-- 						file => routing_handle,
-- 						text => to_string (routing_matrix (column,row)));
-- 				end loop;
-- 				put_lf (file => routing_handle, field_count => routes_total);
-- 				
-- 			end loop;
-- 		end write_routes;
-- 		
-- 		procedure close_routing_table is
-- 		-- Writes the table footer and closes the csv file.
-- 		begin
-- 			-- write file footer
-- 			put_field (file => routing_handle, text => et_csv.row_separator_1);
-- 			put_lf (file => routing_handle, field_count => columns_total);
-- 			
-- 			put_field (file => routing_handle, text => "routing table end");
-- 			put_lf (file => routing_handle, field_count => columns_total);
-- 
-- 			close (routing_handle);
-- 		end close_routing_table;
-- 		
-- 
-- 	begin -- export_routing_tables
-- 		
-- 		log ("exporting routing table ...", log_threshold);
-- 		log_indentation_up;
-- 
-- 		-- Export anything if there are routes at all. Otherwise nothing to do here.
-- 		if routes_total > 0 then
-- 
-- 			-- Adjust number of columns required for csv file.
-- 			-- If more routes required than columns_min (defined by file header)
-- 			-- columns_total is set as number of routes. Otherwise columns_total
-- 			-- is set by the columns_min.
-- 			if routes_total > columns_min then
-- 				columns_total := routes_total;
-- 			else
-- 				columns_total := columns_min;
-- 			end if;
-- 
-- 			-- get the longest route of the routing_table. This value later defines the
-- 			-- length of the routing matrix and thus the number of rows in the csv file.
-- 			longest_route := et_configuration.longest_route (routing_table);
-- 			
-- 			log ("in file " & file_routing_table, log_threshold + 1);
-- 			create_routing_table_header; -- csv file header
-- 
-- 			-- the matrix dimensions will be: width routes_total and length longest_route
-- 			write_routes (create_routing_matrix); 
-- 
-- 			log ("closing file " & file_routing_table, log_threshold + 2);
-- 			close_routing_table;
-- 		else
-- 			log ("no routes found -> nothing to do", log_threshold + 1);
-- 		end if;
-- 			
-- 		log_indentation_down;
-- 	end export_routing_tables;
	
	function to_unit_of_measurement (unit : in string) return type_unit_of_measurement is
	-- Converts a string to type_component_unit_meaning.
		use et_string_processing;
		unit_out : type_unit_of_measurement;
	begin
		unit_out := type_unit_of_measurement'value (unit);
		return unit_out;

		exception
			when others =>
				log (ERROR, unit & " is not a supported unit of measurement !",
					 console => true);

				log (text => "supported units are:");
				for uom in type_unit_of_measurement'pos (type_unit_of_measurement'first) .. 
					type_unit_of_measurement'pos (type_unit_of_measurement'last) loop
					log (text => "- " & to_string (type_unit_of_measurement'val (uom)));
				end loop;
						
				raise constraint_error;
	end to_unit_of_measurement;
	
	function to_string (unit : in type_unit_of_measurement) return string is
	-- returns the given unit of measurement as string. (things like OHM, KILOOHM, MEGAOHM, ...)
	begin
		return type_unit_of_measurement'image (unit);
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
			log (ERROR, "abbrevaton of unit of measurement " 
				& to_string (abbrevation) 
				& " has invalid character at position"
				& natural'image (invalid_character_position),
				console => true);
			raise constraint_error;
		end if;
	end check_abbrevation_of_unit_characters;

	function to_abbrevation (unit : in type_unit_of_measurement) 
	-- Translates from given unit_of_measurement (like OHM or VOLT) to the
	-- actual abbrevation like R or V.
		return type_unit_abbrevation.bounded_string is
		use type_units_of_measurement;
	begin
		return element (type_units_of_measurement.find (component_units, unit));
	end;
	
	function requires_operator_interaction (
		prefix : in et_devices.type_prefix.bounded_string) 
		return type_component_requires_operator_interaction is
	-- Returns YES is given prefix requires operator interaction.
	-- Returns NO if prefixs does not require interaction or if no prefixes
	-- specified at all (in configuration file section COMPONENT_PREFIXES).
		cat : type_device_category;
		use type_categories_with_operator_interacton;
		cat_cursor : type_categories_with_operator_interacton.cursor;
	begin
		if component_prefixes_specified then

			-- get category from given prefix
			cat := category (prefix);
			
			-- search in container component_categories_with_operator_interaction for
			-- category cat.
			cat_cursor := component_categories_with_operator_interaction.find (cat);

			if cat_cursor = type_categories_with_operator_interacton.no_element then
				return NO; -- no operator interaction required
			else
				return YES; -- operator interaction required
			end if;
			
		else  -- no prefixes specified
			return NO;
		end if;
	end requires_operator_interaction;

	function to_text (text : in string) return type_text_schematic is
	-- Converts a string to type_text_schematic.
		use et_string_processing;
		text_out : type_text_schematic;
	begin
		text_out := type_text_schematic'value (text);
		return text_out;

		exception
			when others =>
				log (ERROR, text & " is not a supported text category !",
					 console => true);

				-- show supported text categories
				log (text => "Available categories are :");
				for cat in type_text_schematic'pos (type_text_schematic'first) .. type_text_schematic'pos (type_text_schematic'last) loop
					log (text => "- " & type_text_schematic'image (type_text_schematic'val (cat)));
				end loop;
				
				raise constraint_error;
	end to_text;
	
	function to_string (text : in type_text_schematic) return string is
	-- returns the given text type as string.
	begin
		return type_text_schematic'image (text);
	end to_string;

	procedure check_schematic_text_size (
		category 	: in type_text_schematic;
		size		: in et_coordinates.pac_geometry_sch.type_distance_positive) is
	-- Checks the given text size by its category. Does nothing if no text sizes
	-- specified in configuration file in section TEXT_SIZES_SCHEMATIC.
		use et_string_processing;
		use et_coordinates;
		use et_coordinates.pac_geometry_sch;
		use type_text_sizes_schematic;
		cursor : type_text_sizes_schematic.cursor; -- points to a text size 
	
	begin -- check_schematic_text_size
		-- nothing happens if no text sizes specified
		if not is_empty (text_sizes_schematic) then

			-- Locate text size by given category. If there is
			-- no specification in configuration file, nothing happens.
			cursor := text_sizes_schematic.find (category);
			if cursor /= no_element then
			
				if size /= element (cursor) then
					log (WARNING, "Text size " & to_string (size) 
						& " invalid for category " & to_string (category) 
						& " ! " & "Expected size " & to_string (element (cursor)) 
						& " ! "); --(equals " & to_mil_string (element (cursor)) & " mil)");
				end if;

			end if;
		end if;
		
	end check_schematic_text_size;


-- 	function to_string (partcode : in type_partcode.bounded_string) return string is begin
-- 		return type_partcode.to_string (partcode);
-- 	end to_string;
-- 
-- 	function to_partcode (partcode : in string) return type_partcode.bounded_string is begin
-- 		return type_partcode.to_bounded_string (partcode);
-- 	end to_partcode;
-- 	
-- 	procedure check_partcode_length (partcode : in string) is
-- 	-- Tests if the given partcode is longer than allowed.
-- 		use et_string_processing;
-- 	begin
-- 		if partcode'length > component_partcode_length_max then
-- 			log_indentation_reset;
-- 			log (message_error & "max. number of characters for part code is" 
-- 				 & positive'image (component_partcode_length_max) & " !",
-- 				 console => true);
-- 			raise constraint_error;
-- 		end if;
-- 	end check_partcode_length;
-- 	
-- 	procedure check_partcode_characters (
-- 		partcode	: in type_partcode.bounded_string;
-- 		characters	: in character_set := component_partcode_characters) is
-- 	-- Tests if the given partcode contains only valid characters as specified
-- 	-- by given character set.
-- 	-- Raises exception if invalid character found.
-- 		use et_string_processing;
-- 		use type_partcode;
-- 		invalid_character_position : natural := 0;
-- 	begin
-- 		invalid_character_position := index (
-- 			source => partcode,
-- 			set => characters,
-- 			test => outside);
-- 
-- 		if invalid_character_position > 0 then
-- 			log_indentation_reset;
-- 			log (message_error & "component part code " & to_string (partcode) 
-- 				 & " has invalid character at position"
-- 				 & natural'image (invalid_character_position),
-- 				console => true
-- 				);
-- 			raise constraint_error;
-- 		end if;
-- 	end check_partcode_characters;

	
	
	function to_partcode_keyword_argument (argument : in string) return type_partcode_keyword_argument.bounded_string is
	-- Converts a string to a type_partcode_keyword_argument.
	begin
		return type_partcode_keyword_argument.to_bounded_string (argument);
	end to_partcode_keyword_argument;

	function to_string (argument : in type_partcode_keyword_argument.bounded_string) return string is
	-- Converts a type_partcode_keyword_argument to a string.
	begin
		return type_partcode_keyword_argument.to_string (argument);
	end to_string;
	
	function to_string (keyword : in type_partcode_keyword.bounded_string) return string is
	-- Converts a type_partcode_keyword to a string.
	begin
		return type_partcode_keyword.to_string (keyword);
	end to_string;
	
	procedure check_partcode_keyword_length (keyword : in string) is
	-- Tests if the given partcode keyword is longer than allowed.
		use et_string_processing;
	begin
		if keyword'length > partcode_keyword_length_max then
			log (ERROR, "max. number of characters for part code keyword is" 
				 & positive'image (partcode_keyword_length_max) & " !",
				 console => true);
			raise constraint_error;
		end if;
	end check_partcode_keyword_length;
	
	procedure check_partcode_keyword_characters (
		keyword		: in type_partcode_keyword.bounded_string;
		characters	: in character_set := partcode_keyword_characters) is
	-- Tests if the given keyword contains only valid characters as specified
	-- by given character set.
	-- Raises exception if invalid character found.
		use et_string_processing;
		use type_partcode_keyword;
		invalid_character_position : natural := 0;
	begin
		-- Test given keyword and get position of possible invalid characters.
		invalid_character_position := index (
			source => keyword,
			set => characters,
			test => outside);

		-- Evaluate position of invalid character.
		if invalid_character_position > 0 then
			log (ERROR, "invalid character in part code keyword '" 
				& to_string (keyword) & "' at position" & natural'image (invalid_character_position) & " !",
				console => true);

			-- CS: show allowed characters
			raise constraint_error;
		end if;

	end check_partcode_keyword_characters;

	procedure validate_partcode_keyword (keyword : in type_partcode_keyword.bounded_string) is
	-- Checks whether given keyword is specified in 
	-- in the configuration file section [PART_CODE_KEYWORDS].
	-- NOTE: Assumes there are keywords specified at all.
		use type_partcode_keywords;
		use type_partcode_keyword;
		cursor : type_partcode_keywords.cursor := partcode_keywords.first;
		valid : boolean := false;
	begin
		while cursor /= type_partcode_keywords.no_element loop
			if key (cursor) = keyword then
				valid := true;
				exit;
			end if;
			next (cursor);
		end loop;

		if not valid then
			log (ERROR, "invalid keyword " & to_string (keyword) & " in part code !");
			log (text => "Available keywords are:");
			cursor := partcode_keywords.first;
			while cursor /= type_partcode_keywords.no_element loop
				log (text => "- " & to_string (key (cursor)));
				next (cursor);
			end loop;

			raise constraint_error;
		end if;
		
	end validate_partcode_keyword;
	
	function to_partcode_keyword (keyword : in string) return type_partcode_keyword.bounded_string is
	-- Converts a string to a type_partcode_keyword.
	begin
		return type_partcode_keyword.to_bounded_string (keyword);
	end to_partcode_keyword;

	function compose_partcode_root (
	-- The root of a partcode in general is something like R_PAC_S_0805_VAL_ .
	-- If optionally the value is provided, it gets appended which would result
	-- in something like R_PAC_S_0805_VAL_100R.
		prefix		: in et_devices.type_prefix.bounded_string;			-- R
		packge		: in et_packages.type_component_package_name.bounded_string;	-- S_0805
		value 		: in et_devices.type_value.bounded_string := et_devices.type_value.to_bounded_string ("")) -- 100R
		return material.type_partcode.bounded_string is

		use et_devices;
		use et_packages;
		use type_prefix;
		use type_component_package_name;
		use type_value;
		use material.type_partcode;

		base : constant material.type_partcode.bounded_string :=
			to_bounded_string (
				et_devices.to_string (prefix)				-- R
				& partcode_keyword_separator				-- _
				& to_partcode_keyword (COMPONENT_PACKAGE)	-- PAC
				& partcode_keyword_separator				-- _
				& et_packages.to_string (packge));			-- S_0805
	begin
		if is_empty (value) then
			return base; -- X_PAC_S_USB-MINI
		else
			return base & to_bounded_string ( -- R_PAC_S_0805_VAL_100R
				partcode_keyword_separator				-- _
				& to_partcode_keyword (COMPONENT_VALUE)	-- VAL
				& partcode_keyword_separator			-- _
				& et_devices.to_string (value));		-- 100R
		end if;
	end compose_partcode_root;

	procedure validate_other_partcode_keywords (
	-- Validates optional keywords as specified in configuration file.
	-- Starts the validation from the given character position.
		partcode		: in material.type_partcode.bounded_string; -- R_PAC_S_0805_VAL_100R_TOL_5_PMAX_0W125
		from			: in positive; -- the character position to start from
		log_threshold	: in et_string_processing.type_log_level) is

		use material;
		use material.type_partcode;
		use type_partcode_keywords;
		use type_partcode_keyword_argument;
		use et_string_processing;
		
		len 		: positive := length (partcode); 	-- the length of the given partcode
		place 		: positive := from; 				-- the position of the character being processed
		keyword_end : positive;							-- the last character position of the current keyword

		keyword_follows : boolean;	-- goes true if a keyword is expected next

		keyword : type_partcode_keyword.bounded_string;	-- the keyword being processed

		argument_start : positive;
		argument : type_partcode_keyword_argument.bounded_string; -- the argument being processed

		procedure validate_argument (
			kw	: in type_partcode_keyword.bounded_string;
			arg	: in type_partcode_keyword_argument.bounded_string) is
		begin
			log (text => "keyword " & to_string (kw) 
				 & " argument " & to_string (argument => argument), level => log_threshold + 1);

			-- CS: currently no validation ! Here the argument could be checked against the keyword
			-- example: after PMAX must follow something like 15 (for 15W watts)
			-- after VMAX must follow 6V3 ...
			-- Instead of comma, use unit of measurement (same scheme as in component value).
			-- The format of the argument should be specified in the configuration file.
		end validate_argument;
			
	begin -- validate_other_partcode_keywords
		log (text => "optional keywords ...", level => log_threshold);
		log_indentation_up;

		-- If the first character to start with, is a separator, then an argument follows.
		-- Otherwise a keyword follows. Example:
		-- _100R_TOL_5_PMAX_0W125 -> argument follows
		-- x_TOL_5_PMAX_0W125 -> keyword follows
		if element (partcode, place) = partcode_keyword_separator then
			keyword_follows := false;
		else
			keyword_follows := true;
		end if;

		-- advance through the partcode characters
		while place < len loop

			if keyword_follows then
				--log (text => "reading keyword", level => log_threshold + 1);
				
				if element (partcode, place) = partcode_keyword_separator then
					place := place + 1;
					keyword_end := type_partcode.index (partcode, (1 => partcode_keyword_separator), from => place) - 1;
					
					keyword := to_partcode_keyword (slice (partcode, place, keyword_end));
					log (text => "keyword " & enclose_in_quotes (to_string (keyword)), level => log_threshold + 2);
					
					place := keyword_end + 1; -- point to separator right after keyword
					argument_start := place + 1; -- so the argument is expected after the separator
					
					keyword_follows := false;
					validate_partcode_keyword (keyword);
					
					-- A keyword must occur only once:
					if type_partcode.count (partcode, to_string (keyword)) > 1 then
						log (WARNING, "keyword " & enclose_in_quotes (to_string (keyword)) & " can be used only once !");
					end if;
				else
					place := place + 1;	-- next character of keyword
				end if;

			else -- argument follows
				--log ("reading argument", level => log_threshold + 1);
				
				place := place + 1;
				-- If the argument starts, "place" points to the first character of the argument.

				-- If a separator occurs, the argument ends.
				if element (partcode, place) = partcode_keyword_separator then

					-- detect missing argument
					if place = argument_start then
						log (WARNING, "expect argument after keyword at position" & positive'image (place) & " !");
					end if;
					
					keyword_follows := true;

					-- The argument can now be sliced from argument_start to the place before the separator:
					argument := to_partcode_keyword_argument (material.type_partcode.slice (partcode, argument_start, place - 1));
					validate_argument (keyword, argument);
					
				elsif place = len then -- last argument in partcode
					
					-- The argument can now be sliced from argument_start to the end of the partcode:
					argument := to_partcode_keyword_argument (material.type_partcode.slice (partcode, argument_start, place));
					validate_argument (keyword, argument);
				end if;

			end if;
			
		end loop;

		log_indentation_down;

		exception
			when event:
				others =>
				log (WARNING, "Error in optional keywords of partcode " & 
					 enclose_in_quotes (material.to_string (partcode)) &
					 " at position" & positive'image (place) & " !");
				
				log (text => ada.exceptions.exception_message (event));
		
	end validate_other_partcode_keywords;

	procedure validate_partcode (
	-- Tests if the given partcode of a device is correct.
	-- The given properties are assumed to be those of a real device.
	--  - If partcode keywords are not specified in the 
	--    configuration file, nothing is validated. It is the users responsibility 
	--    to specify a correct partcode.
	--  - If partcode keywords are specified in the configuration file,
	--    the root part (like R_PAC_S_0805_VAL_) is validated.
		partcode		: in material.type_partcode.bounded_string; -- R_PAC_S_0805_VAL_100R
		device_name		: in type_name;						-- R45
		packge			: in et_packages.type_component_package_name.bounded_string;	-- S_0805
		value 			: in type_value.bounded_string;			-- 100R
		log_threshold	: in et_string_processing.type_log_level)
		is

		use et_string_processing;
		use material.type_partcode;

		place : natural;
		partcode_root : material.type_partcode.bounded_string;
		
		procedure partcode_invalid is begin
			log (WARNING, "device " & to_string (device_name)
				 & " partcode invalid ! Found " & enclose_in_quotes (to_string (partcode)) &
				". Expected " & enclose_in_quotes (to_string (partcode_root)) & " !");
		end partcode_invalid;

		use material;
		
	begin -- validate_partcode
		if partcode_keywords_specified then
			
			log (text => "checking partcode against device name, package and value ...", level => log_threshold);
			log_indentation_up;

			-- Compose the root of the partcode as it should be.
			-- The root is usually something like R_PAC_S_0805_VAL_100R which contains
			-- the given prefix, package name and - if provided - the value.
			partcode_root := compose_partcode_root (
				prefix	=> device_name.prefix,
				packge	=> packge,
				value	=> value);

			-- The root of the partcode must be the very first part of the given partcode.
			-- In that case other keywords can be checked.
			-- If the root partcode is somewhere else or too long, issue warning.
			place := index (partcode, material.to_string (partcode_root));
			
			if place = 1 and length (partcode) = length (partcode_root) then

				-- After the root partcode (like R_PAC_S_0805_VAL_100R) other
				-- keywords may follow:
				validate_other_partcode_keywords (
					partcode		=> partcode, -- the partcode to be validated
					from			=> length (partcode_root), -- last character position of root part code
					log_threshold	=> log_threshold + 1);
				
			else
				partcode_invalid;
			end if;

			log_indentation_down;
		end if;
	end validate_partcode;
	
	function to_partcode_section (text : in string) return type_partcode_section is
	-- converts a string to a type_partcode_section.
	begin
		return type_partcode_section'value (text);

		exception
			when others =>
				log (ERROR, text & " is not a supported partcode section !",
					 console => true);

				-- show supported sections
				log (text => "Available sections are :");
				for section in type_partcode_section'pos (type_partcode_section'first) .. type_partcode_section'pos (type_partcode_section'last) loop
					log (text => "- " & type_partcode_section'image (type_partcode_section'val (section)));
				end loop;
				
				raise constraint_error;

	end to_partcode_section;

	function to_string (partcode_section : in type_partcode_section) return string is
	-- converts a type_partcode_section to a string.
	begin
		return type_partcode_section'image (partcode_section);
	end to_string;

	function partcode_keywords_specified return boolean is
	-- Returns true if any part code keywords are specified via configuration file.
		use type_partcode_keywords;
	begin
		if is_empty (partcode_keywords) then -- no keywords specified
			return false;
		else -- keywords are specified
			return true;
		end if;
	end partcode_keywords_specified;
	
	function to_partcode_keyword (section : in type_partcode_section) return string is
	-- Returns for the given partcode section the corresponding keyword as specified
	-- in the configuration file section [PART_CODE_KEYWORDS].
	-- If no keyword specified (or no conf. file applied) returns an empty string.
		keyword : type_partcode_keyword.bounded_string;
		use type_partcode_keywords;
		cursor : type_partcode_keywords.cursor;
	begin
		if partcode_keywords_specified then

			-- Search in partcode_keywords the given section name and
			-- load keyword.
			cursor := partcode_keywords.first;
			while cursor /= type_partcode_keywords.no_element loop
				if element (cursor) = section then -- MAXIMUM_POWER
					keyword := key (cursor); -- PMAX
					exit; -- no further search required
				end if;
				next (cursor);
			end loop;
			
		end if;
		return type_partcode_keyword.to_string (keyword);
	end to_partcode_keyword;
	
	procedure make_default_conventions (
		file_name		: in type_conventions_file_name.bounded_string;
		log_threshold	: in et_string_processing.type_log_level) is
	-- Creates a default conventions file.
		use et_general;
		use et_coordinates.pac_geometry_sch;
		
		function comment return string is begin return comment_mark & latin_1.space; end comment;

		conventions_file_handle : ada.text_io.file_type;
	begin
		log (text => "generating default conventions file " & to_string (file_name), level => log_threshold);

		if exists (to_string (file_name)) then
			-- CS: warn operator and request confirmation
			null;
		end if;
		
		create (
			file => conventions_file_handle, 
			mode => out_file, 
			name => to_string (file_name));

		set_output (conventions_file_handle);
		
		put_line (comment & system_name & " conventions");
		put_line (comment & "auto generated by "
			& system_name & latin_1.space & version & " " & date);
		put_line (comment & "Please modify it according to your needs.");
		put_line (comment & row_separator_double);
		new_line;

		-- DEVICE PREFIXES
		put_line (section_component_prefixes); -- section header
		new_line;		
		put_line (comment & "prefix category");
		new_line;		
		put_line ("ANT " & to_string (ANTENNA)); -- CS: short desciption as comment for all
		put_line ("B   " & to_string (BUZZER));
		put_line ("BAT " & to_string (BATTERY));
		put_line ("C   " & to_string (CAPACITOR));
		put_line ("CA  " & to_string (CAPACITOR_ADJUSTABLE));
		--put_line (configuration_file_handle, "CBL" & to_string (CABLE));
		put_line ("D   " & to_string (DIODE));
		put_line ("DPH " & to_string (DIODE_PHOTO));
		put_line ("DI  " & to_string (DIAC));
		put_line ("DIS " & to_string (DISPLAY));
		put_line ("F   " & to_string (FUSE));
		put_line ("HS  " & to_string (HEATSINK));
		put_line ("IC  " & to_string (INTEGRATED_CIRCUIT));
		put_line ("J   " & to_string (JUMPER));
		put_line ("JD  " & to_string (JUMPER));
		put_line ("K   " & to_string (RELAY));
		put_line ("KP  " & to_string (KEYPAD));
		put_line ("L   " & to_string (INDUCTOR));
		put_line ("LA  " & to_string (INDUCTOR_ADJUSTABLE));
		put_line ("LS  " & to_string (LOUDSPEAKER));
		put_line ("LED " & to_string (LIGHT_EMMITTING_DIODE));
		put_line ("LDA " & to_string (LIGHT_EMMITTING_DIODE_ARRAY));
		put_line ("M   " & to_string (MOTOR));
		put_line ("MIC " & to_string (MICROPHONE));
		put_line ("N   " & to_string (NETCHANGER));
		put_line ("OC  " & to_string (OPTOCOUPLER));		
		put_line ("Q   " & to_string (QUARTZ));
		put_line ("R   " & to_string (RESISTOR));
		put_line ("RA  " & to_string (RESISTOR_ADJUSTABLE));
		put_line ("RN  " & to_string (RESISTOR_NETWORK));
		put_line ("RP  " & to_string (POTENTIOMETER));
		put_line ("RPH " & to_string (RESISTOR_PHOTO));
		put_line ("S   " & to_string (SWITCH));
		put_line ("T   " & to_string (TRANSISTOR));
		put_line ("TP  " & to_string (TRANSISTOR_PHOTO));
		put_line ("TF  " & to_string (TRANSFORMER));
		put_line ("TPT " & to_string (TESTPOINT));
		put_line ("TH  " & to_string (THYRISTOR));
		put_line ("THP " & to_string (THYRISTOR_PHOTO));
		put_line ("TR  " & to_string (TRIAC));
		put_line ("TUB " & to_string (TUBE));		
		--put_line (configuration_file_handle, "W " & to_string (WIRE));
		put_line ("X   " & to_string (CONNECTOR));
		put_line ("XD  " & to_string (CONNECTOR));
		
		new_line;
		new_line;		

		-- UNITS OF MEASUREMENT
		put_line (section_component_units); -- section header
		new_line;
		put_line (comment & "abbrevation unit_of_measurement");
		new_line;
		put_line ("m " & to_string (MILLIOHM));
		put_line ("R " & to_string (OHM));
		put_line ("k " & to_string (KILOOHM));
		put_line ("M " & to_string (MEGAOHM));
		put_line ("G " & to_string (GIGAOHM));		

		put_line ("p " & to_string (PICOFARAD));		
		put_line ("n " & to_string (NANOFARAD));
		put_line ("u " & to_string (MICROFARAD));
		put_line ("m " & to_string (MILLIFARAD));
		put_line ("F " & to_string (FARAD));

		put_line ("n " & to_string (NANOHENRY));		
		put_line ("u " & to_string (MICROHENRY));
		put_line ("m " & to_string (MILLIHENRY));
		put_line ("H " & to_string (HENRY));

		put_line ("V " & to_string (VOLT));

		put_line ("m " & to_string (MILLIAMPERE));		
		put_line ("A " & to_string (AMPERE));

		put_line ("k " & to_string (KILOHERTZ));
		put_line ("M " & to_string (MEGAHERTZ));
		put_line ("G " & to_string (GIGAHERTZ));
		
		new_line;
		new_line;		

		-- DEVICES THAT REQUIRE OPERATOR INTERACTION
		put_line (section_components_with_operator_interaction); -- section header
		new_line;		
		put_line (comment & "category");
		new_line;		
		put_line (to_string (BUZZER));
		put_line (to_string (CAPACITOR_ADJUSTABLE));
		put_line (to_string (CONNECTOR));
		put_line (to_string (DIODE_PHOTO));
		put_line (to_string (DISPLAY));
		put_line (to_string (FUSE));
		put_line (to_string (INDUCTOR_ADJUSTABLE));
		put_line (to_string (JUMPER));
		put_line (to_string (KEYPAD));
		put_line (to_string (LIGHT_EMMITTING_DIODE));
		put_line (to_string (LIGHT_EMMITTING_DIODE_ARRAY));
		put_line (to_string (RESISTOR_ADJUSTABLE));
		put_line (to_string (RESISTOR_PHOTO));
		put_line (to_string (POTENTIOMETER));
		put_line (to_string (SWITCH));
		put_line (to_string (TESTPOINT));
		put_line (to_string (THYRISTOR_PHOTO));

		new_line;
		new_line;
		
		-- TEXT SIZES IN SCHEMATIC
		put_line (section_text_sizes_schematic); -- section header
		new_line;
		put_line (comment & "sizes for various kinds of texts in schematic"); 
		put_line (comment & "category" & latin_1.space & "mm");
		new_line;		
		put_line (to_string (NET_LABEL)	& to_string (et_schematic.net_label_text_size_default));
		put_line (to_string (PORT_NAME) & to_string (et_symbols.text_size_default));
		put_line (to_string (TERMINAL_NAME) & to_string (et_symbols.text_size_default));
		put_line (to_string (COMPONENT_ATTRIBUTE) & to_string (et_symbols.text_size_default));
		put_line (to_string (SHEET_NAME) & to_string (et_project.sheet_name_text_size_default)); -- CS no longer required ?
		put_line (to_string (conventions.FILE_NAME) & to_string (et_project.file_name_text_size_default));
		
		new_line;
		new_line;
		
		-- CONNECTOR GND TERMINAL
        -- CS
-- 		put_line (configuration_file_handle, section_connector_gnd_terminal); -- section header
-- 		new_line (configuration_file_handle);

		-- CS LINE WIDTHS

		-- PARTCODE KEYWORDS
		put_line (section_partcode_keywords); -- section header
		new_line;
		put_line (comment & "sections in device part code and their keywords"); 
		put_line (comment & "keyword" & latin_1.space & "section");
		new_line;
		put_line ("PAC   " & to_string (COMPONENT_PACKAGE));
		put_line ("VAL   " & to_string (COMPONENT_VALUE));
		put_line ("TOL   " & to_string (TOLERANCE));
		put_line ("VMAX  " & to_string (MAXIMUM_VOLTAGE));
		put_line ("PMAX  " & to_string (MAXIMUM_POWER));
		put_line ("TYPE  " & to_string (PART_TYPE));
		put_line ("PN    " & to_string (PART_NUMBER));

		new_line;
		new_line;

		put_line (comment & row_separator_double);		
		put_line (comment & system_name & " conventions end");

		set_output (standard_output);
		close (conventions_file_handle);

		exception
			when event:
				others =>
					set_output (standard_output);
					put_line (message_error & "Read export report for warnings and error messages !"); -- CS: show path to report file
					raise;
		
	end make_default_conventions;


	procedure read_conventions (
		file_name		: in type_conventions_file_name.bounded_string;
		log_threshold	: in et_string_processing.type_log_level) is
	-- Reads the given conventions file.

		use et_string_processing;
		line : et_string_processing.type_fields_of_line; -- the line of the file

		conventions_file_handle : ada.text_io.file_type;
		
		type type_section is (
			NONE,
			COMPONENT_PREFIXES,
			COMPONENT_UNITS,
			COMPONENTS_WITH_OPERATOR_INTERACTION,
			TEXT_SIZES_SCHEMATIC,
			PARTCODE_KEYWORDS
			);
		
		section_entered : type_section := NONE;
		
		-- lines of the file are collected in a simple list:
		package type_lines is new doubly_linked_lists (
			element_type	=> et_string_processing.type_fields_of_line,
			"="				=> et_string_processing.lines_equally);
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
		
			use et_devices;
			use et_symbols;
			use et_coordinates;
			use et_coordinates.pac_geometry_sch;
			
			procedure test_multiple_occurences is begin
				if not inserted then
					log (WARNING, affected_line (element (line_cursor)) & "multiple occurence of assignment ! Entry ignored !");
				end if;
			end test_multiple_occurences;

			function reduced_check_coverage return string is begin 
				return " Design check coverage reduced !";
			end reduced_check_coverage;
			
			prefix 		: type_prefix.bounded_string;
			cat 		: type_device_category;
			
			abbrevation	: type_unit_abbrevation.bounded_string;
			unit		: type_unit_of_measurement;

			text		: type_text_schematic;
			size		: et_symbols.pac_text.type_text_size;

			partcode_keyword	: type_partcode_keyword.bounded_string;
			partcode_section	: type_partcode_section;
			
			-- CS: check field count in sections respectively. issue warning if too many fields. 
		begin -- process_previous_section
			next (line_cursor); -- the first line of the section is its header itself and can be skipped
			log_indentation_up;

			case section_entered is
				when NONE => null;

				when COMPONENT_PREFIXES =>
					log (text => "device prefixes ...", level => log_threshold + 1);
					log_indentation_up;

					while line_cursor /= type_lines.no_element loop
						log (text => to_string (element (line_cursor)), level => log_threshold + 2);

						-- Build the prefix from field #1:
						-- Test if prefix is not too long, if it contains only allowed characters.
						-- We test against the default character set as specified in et_libraries.
						check_prefix_length (et_string_processing.field (element (line_cursor), 1));
						prefix := type_prefix.to_bounded_string (et_string_processing.field (element (line_cursor), 1));
						check_prefix_characters (prefix);

						-- build the component category from field #2:
						cat := to_category (et_string_processing.field (element (line_cursor), 2));
						
						-- insert the prefix assignment in container component_prefixes
						type_component_prefixes.insert (
							container	=> conventions.component_prefixes,
							position	=> component_prefix_cursor,
							key 		=> prefix,
							new_item 	=> cat,
							
							-- If entry already in map, this flag goes true. Warning issued later. see below.
							inserted => inserted);

						test_multiple_occurences;
						next (line_cursor);
					end loop;

					-- Notify operator if no prefixes specified:
					if type_component_prefixes.is_empty (conventions.component_prefixes) then
						log (WARNING, "no device prefixes specified !" & reduced_check_coverage);
					end if;
					
					log_indentation_down;

				-- COMPONENT UNITS OF MEASUREMENT
				when component_units =>
					log (text => "device units of measurement ...", level => log_threshold + 1);
					log_indentation_up;
					
					while line_cursor /= type_lines.no_element loop
						log (text => to_string (element (line_cursor)), level => log_threshold + 2);

						-- Build the unit abbrevation from field #1:
						-- Test if abbrevation contains only allowed characters.
						-- We test against the character set specified for abbrevations of units of measurement.
						abbrevation := type_unit_abbrevation.to_bounded_string (et_string_processing.field (element (line_cursor), 1));
						check_abbrevation_of_unit_characters (abbrevation, unit_abbrevation_characters);

						-- Build the unit of measurement from field #2:
						unit := to_unit_of_measurement (et_string_processing.field (element (line_cursor), 2));
						
						-- insert the abbrevation to unit of measurement assignment in container component_units
						type_units_of_measurement.insert (
							container	=> conventions.component_units,
							position	=> unit_cursor,

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

					-- Notify operator if no units of measurement specified:
					if type_units_of_measurement.is_empty (conventions.component_units) then
						log (WARNING, "no units of measurement specified !" & reduced_check_coverage);
					end if;
					log_indentation_down;

				-- COMPONENTS WITH USER INTERACTON
				when components_with_operator_interaction =>
					log (text => "device categories with operator interaction ...", level => log_threshold + 1);
					log_indentation_up;

					if not component_prefixes_specified then
						log (WARNING, "section " & section_component_prefixes & " empty or missing !");
						log (WARNING, "section " & section_components_with_operator_interaction & " without effect !");
					end if;
					
					while line_cursor /= type_lines.no_element loop
						log (text => to_string (element (line_cursor)), level => log_threshold + 2);

						-- build the component category from field #1:
						cat := to_category (et_string_processing.field (element (line_cursor), 1));

						-- insert the category in container component_categories_with_operator_interaction
						type_categories_with_operator_interacton.insert (
							container => component_categories_with_operator_interaction,
							new_item => cat);
						
						next (line_cursor);
					end loop;

					-- Notify operator if no components specified:
					if type_categories_with_operator_interacton.is_empty (conventions.component_categories_with_operator_interaction) then
						log (WARNING, "no categories specified !" & reduced_check_coverage);
					end if;
					log_indentation_down;

				-- TEXT SIZES SCHEMATIC
				when text_sizes_schematic =>
					log (text => "text sizes in schematic ...", level => log_threshold + 1);
					log_indentation_up;
					
					while line_cursor /= type_lines.no_element loop
						log (text => to_string (element (line_cursor)), level => log_threshold + 2);

						-- build the text category from field #1:
						text := to_text (et_string_processing.field (element (line_cursor), 1));

						-- build the text size from field #2. 
						-- Depending on the text category the string is passed through
						-- the corresponding text size subtypes for that category:
						case text is
							when NET_LABEL =>
								size := et_schematic.to_net_label_text_size (et_string_processing.field (element (line_cursor), 2));

							when PORT_NAME =>
								size := to_distance (et_string_processing.field (element (line_cursor), 2));

							when TERMINAL_NAME =>
								size := to_distance (et_string_processing.field (element (line_cursor), 2));

							when COMPONENT_ATTRIBUTE =>
								size := to_distance (et_string_processing.field (element (line_cursor), 2));

							when SHEET_NAME =>
								size := et_project.to_sheet_name_text_size (et_string_processing.field (element (line_cursor), 2));

							when conventions.FILE_NAME =>
								size := et_project.to_file_name_text_size (et_string_processing.field (element (line_cursor), 2));
								
						end case;
						
						-- insert the text category and size in container text_sizes_schematic
						type_text_sizes_schematic.insert (
							container	=> conventions.text_sizes_schematic,
							key 		=> text,
							new_item 	=> size);
						
						next (line_cursor);
					end loop;

					-- Notify operator if no sizes specified:
					if type_text_sizes_schematic.is_empty (conventions.text_sizes_schematic) then
						log (WARNING, "no text sizes specified !" & reduced_check_coverage);
					end if;
					log_indentation_down;

				-- PARTCODE KEYWORDS
				when partcode_keywords =>
					log (text => "part code keywords ...", level => log_threshold + 1);
					log_indentation_up;
					
					while line_cursor /= type_lines.no_element loop
						log (text => to_string (element (line_cursor)), level => log_threshold + 2);

						-- build the partcode keyword from field #1:
						check_partcode_keyword_length (et_string_processing.field (element (line_cursor), 1));
						partcode_keyword := to_partcode_keyword (et_string_processing.field (element (line_cursor), 1));
						check_partcode_keyword_characters (partcode_keyword);

						-- build the partcode section name from field #2. 
						partcode_section := to_partcode_section (et_string_processing.field (element (line_cursor), 2));
						
						-- insert the text category and size in container text_sizes_schematic
						type_partcode_keywords.insert (
							container	=> conventions.partcode_keywords,
							key 		=> partcode_keyword,
							new_item 	=> partcode_section);
						
						next (line_cursor);
					end loop;

					-- Notify operator if no keywrds specified:
					if type_partcode_keywords.is_empty (conventions.partcode_keywords) then
						log (WARNING, "no part code keywords specified !" & reduced_check_coverage);
					end if;
					log_indentation_down;

			end case;
			
			log_indentation_down;

			-- clean up. empty container "lines" for next section
			lines.clear;

			exception
				when others =>
					log (ERROR, affected_line (element (line_cursor)) & latin_1.space & to_string (element (line_cursor)),
						 console => true);

					-- CS: provide information on what is wrong with the line (depending on section_entered)
					-- number of characters in unit of measurement
					-- propose category, units, ...
					
					raise;
					
		end process_previous_section;
		
	begin -- read_conventions
		log (text => row_separator_double, level => log_threshold);		
		log (text => "reading conventions file " & to_string (file_name) & " ...", level => log_threshold);

		if exists (to_string (file_name)) then

			conventions.component_prefixes := type_component_prefixes.empty_map;

			open (file => conventions_file_handle, mode => in_file, name => to_string (file_name));
			set_input (conventions_file_handle);

			-- read configuration file line per line
			while not end_of_file loop

				-- Store line in variable "line" (see et_string_processing.ads)
				line := et_string_processing.read_line (
					line			=> get_line,
					number			=> ada.text_io.line (current_input),
					comment_mark	=> comment_mark,
					delimiter_wrap	=> true, -- if connector purpose is given in quotations
					ifs 			=> latin_1.space); -- fields are separated by space

				case field_count (line) is
					when 0 => null; -- we skip empty lines
					when others =>

						-- CS: check field count ?
						
						-- At a certain log level we report the whole line as it is:
						--log (text => to_string (line), level => log_threshold + 3);

						-- Wait for a section header.
						-- Once a header was found, the PREVIOUS section is regarded as complete.
						-- The PREVIOUS section is then processed with all its lines in container "lines".
						-- Set section_entered according to the section BEING entered.

						if et_string_processing.field (line, 1) = section_component_prefixes then
							process_previous_section;
							section_entered := component_prefixes;
						end if;

						if et_string_processing.field (line, 1) = section_component_units then
							process_previous_section;
							section_entered := component_units;
						end if;

						if et_string_processing.field (line, 1) = section_components_with_operator_interaction then
							process_previous_section;
							section_entered := components_with_operator_interaction;
						end if;

						if et_string_processing.field (line, 1) = section_text_sizes_schematic then
							process_previous_section;
							section_entered := text_sizes_schematic;
						end if;

						if et_string_processing.field (line, 1) = section_partcode_keywords then
							process_previous_section;
							section_entered := partcode_keywords;
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
			close (conventions_file_handle);
			
		else
			log (ERROR, "conventions file " & to_string (file_name) & " not found !",
				 console => true);
			raise constraint_error;
		end if;

	end read_conventions;

	function value_valid (
	-- Tests if the given device value meets certain conventions.
	-- This test depends on the category of the device. If no prefixes specified
	-- in the configuration file, this test does nothing.
	-- Returns false if any violation has been detected.							 
	-- CS: If value is 10,0R outputs the same warning multiple times. Rework required.
		value 	: in et_devices.type_value.bounded_string; -- 100R, 1A5
		prefix	: in et_devices.type_prefix.bounded_string) -- R, F
		return boolean is

		-- This flag goes false once an error has been detected.
		result : boolean := true;
		
		use et_string_processing;
		use et_devices;
		use type_value;
		
		component_category : type_device_category;
		value_length : natural := type_value.length (value);

		procedure value_invalid is begin
			log (WARNING, "value " & enclose_in_quotes (et_devices.to_string (value)) &
				" invalid ! Check unit of measurement !");
			result := false;			
		end;

		procedure no_value is begin
			log (WARNING, "no value found !");
			result := false;
		end;
		
		procedure unit_of_measurement_valid is
		-- Tests if the unit of measurement is valid and placed properly in something like 220k56 .
		-- Tests if the first character is a digit.
		-- Sets the result to false on first error and exits prematurely.
			use ada.strings.maps.constants;
			place		: positive := 1; -- the pointer to the character being examined
			char 		: character; -- the character being examined

			-- goes true once a valid abbrevation of a unit of measurement is found
			unit_ok 	: boolean := false; 
		
			use type_unit_abbrevation;
			use type_units_of_measurement;
		
			function valid (unit : in type_unit_of_measurement) 
				return boolean is
			-- Sets unit_ok flag true if the given abbrevation starts at position "place".
			-- If so, sets "place" to the position of the last character of the unit.
			-- If at "place" unit not found, set result to false.
				abbrevation : type_unit_abbrevation.bounded_string := to_abbrevation (unit);
			begin
				if index (value, to_string (abbrevation), place) = place then
					-- abbrevation valid. advance place to end of abbrevation.
					place := place + length (abbrevation) - 1;
					unit_ok := true;
					return true;
				else
					-- unit invalid.
					return false;
				end if;
			end;
						
		begin -- unit_of_measurement_valid
			-- We process one character after another in the given value.
			while place <= value_length and result = true loop
				char := element (value, place);

				-- Test if first character is a digit.
				if place = 1 and not is_digit (char) then 
					value_invalid; -- sets result to false
					exit; -- no need to proceed. cancel immediately
				end if;
				
				-- Initially we assume that there has no unit of measurement been found.
				-- So we advance until the first non-digit character is found.
				-- Once a valid abbrevation (like R,k or V) was found, we expect ONLY digits 
				-- after the last character of the unit of measurement. 
				-- Example: in 220k56 all characters after k must be digits.
				if not unit_ok then
				
					if not is_digit (char) then -- integer part like 220 complete
						-- Now the abbrevation for the unit of measurement shall begin
						-- at the position where "place" is pointing at.

						case component_category is

							when BATTERY =>
								if not valid (VOLT)
									then result := false;
								end if;
								
							when CAPACITOR =>
								if not (valid (PICOFARAD)
									or valid (NANOFARAD)
									or valid (MICROFARAD)
									or valid (MILLIFARAD)
									or valid (FARAD)) then 
										result := false;
								end if;

							when FUSE =>
								if not (valid (MILLIAMPERE)
									or valid (AMPERE)) then 
										result := false;
								end if;

							when INDUCTOR =>
								if not (valid (NANOHENRY)
									or valid (MICROHENRY)
									or valid (MILLIHENRY)
									or valid (HENRY)) then 
										result := false;
								end if;

							when RESISTOR | RESISTOR_NETWORK =>
								if not (valid (MILLIOHM)
									or valid (OHM)
									or valid (KILOOHM)
									or valid (MEGAOHM)
									or valid (GIGAOHM)) then 
										result := false;
								end if;

							when QUARTZ =>
								if not (valid (KILOHERTZ)
									or valid (MEGAHERTZ)
									or valid (GIGAHERTZ)) then 
										result := false;
								end if;

							when others => null;
								
						end case;
					end if;

				else 
					-- Unit has been found valid. 
					-- Expect ONLY trailing digits after the unit of measurement.
					if not is_digit (char) then
						value_invalid; -- sets result false
						exit; -- no need to proceed. cancel immediately
					end if;
				end if;

				-- advance to next character in given value
				place := place + 1;
			end loop;

			-- Now all characters have been tested. 
			-- If no valid unit of measurement found, set result false.
			if unit_ok then
				result := true;
			else
				value_invalid; -- sets result false
			end if;

		end unit_of_measurement_valid;

	begin -- validate_value
		-- Do the test if component prefixes specified. Otherwise do nothing.
		if component_prefixes_specified then
	
			-- If a value is provided, means it has non-zero length we conduct some tests.
			-- If no value provided, the category determines whether to abort or not.
			if value_length > 0 then

				-- Units of measurement must be in accordance with the component category
				component_category := category (prefix);
				
				-- For certain component categories the value must start 
				-- with a digit (like 3n3, 1V8, ...):
				case component_category is
					when BATTERY | CAPACITOR | FUSE | INDUCTOR | RESISTOR | RESISTOR_NETWORK | QUARTZ => -- CS: others ?
						unit_of_measurement_valid; -- sets result false on error

					when others => 
						result := true;
				end case;

			else
				-- no value provided (zero length)
				
				-- For certain component categories there is no need for a value. The properties of such parts
				-- are available via the part code.
				-- For other categories (R, L, C, ...) the value is essential for reading and understanding the schematic.
				case category (prefix) is

					-- no value required for:
					when HEATSINK | JUMPER | MOTOR | MICROPHONE | NETCHANGER | SWITCH | TESTPOINT | CONNECTOR =>
						result := true;

					-- value essential for all other categories:
					when others => 
						no_value; -- sets return to false
					
				end case;
			end if;

		else
			result := true;
		end if;

		return result;
		
		exception
			when others => 
				-- CS: explain more detailled what is wrong
				value_invalid;
				return false;

	end value_valid;

	
	function prefix_valid (prefix : in et_devices.type_prefix.bounded_string) return boolean is
	-- Tests if the given prefix is valid as specified in the configuration file.
	-- Raises warning if not and returns false. 
	-- Returns true if no prefixes specified or if prefix is valid.
		use et_devices.type_prefix;
		use type_component_prefixes;
		result : boolean := true;
	begin
		-- if there are prefixes specified, test if the given particular prefix is among them
		if component_prefixes_specified then
			if component_prefixes.find (prefix) = type_component_prefixes.no_element then
				log (WARNING, "invalid prefix " & et_devices.to_string (prefix) & " !");
				result := false;
			end if;
		end if;

		return result;
	end prefix_valid;
	
	function prefix_valid (reference : in type_name) return boolean is
	-- Tests if the given device name has a valid prefix as specified in the configuration file.
	-- Raises warning if not and returns false. 
	-- Returns true if no prefixes specified or if prefix is valid.
		use type_prefix;
		use type_component_prefixes;
		result : boolean := true;
	begin
		-- if there are prefixes specified, test if the given particular prefix is among them
		if component_prefixes_specified then
			if component_prefixes.find (reference.prefix) = type_component_prefixes.no_element then
				log (WARNING, "invalid prefix in device name "
					 & et_devices.to_string (reference) & " !");
				result := false;
			end if;
		end if;

		return result;
	end prefix_valid;

	
end conventions;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
