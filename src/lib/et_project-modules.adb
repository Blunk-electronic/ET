------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                           PROJECT MODULES                                --
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

--   For correct displaying set tab with in your edtior to 4.

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
with ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;
with ada.strings; 				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;

with ada.exceptions;
with ada.directories;
with gnat.directory_operations;

with ada.containers;            use ada.containers;
with ada.containers.ordered_maps;

with et_export;
with et_text;					use et_text;
with et_general_rw;				use et_general_rw;
with et_meta;

with et_pcb_coordinates;
with et_geometry;				use et_geometry;
with et_symbols;

with et_symbol_rw;				use et_symbol_rw;
with et_device_rw;				use et_device_rw;
with et_pcb_rw;					use et_pcb_rw;
with et_schematic_rw;			use et_schematic_rw;
with et_schematic_ops;			use et_schematic_ops;
with et_material;

with et_conventions;
with et_project.rigs;
with et_exceptions;				use et_exceptions;

package body et_project.modules is

	use et_general.type_net_name;
	use pac_generic_modules;
	
	function to_string (project_name : in pac_project_name.bounded_string) return string is
	begin
		return pac_project_name.to_string (project_name);
	end to_string;
	
	function to_project_name (name : in string) return pac_project_name.bounded_string is
	-- Converts the given string to pac_project_name.
	begin
		return pac_project_name.to_bounded_string (name);
	end to_project_name;

	function to_string (path : in type_et_project_path.bounded_string) return string is begin
		return type_et_project_path.to_string (path);
	end to_string;

	function to_project_path (path : in string) return type_et_project_path.bounded_string is begin
		return type_et_project_path.to_bounded_string (path);
	end to_project_path;
	
	function exists (module : in type_module_name.bounded_string) return boolean is begin
	-- Returns true if the module with the given name exists in container modules.
		return pac_generic_modules.contains (generic_modules, module);
	end;

	function locate_module (name : in type_module_name.bounded_string) -- motor_driver (without extension *.mod)
	-- Locates the given module in the global container "generic_modules".
		return pac_generic_modules.cursor is
	begin
		return find (generic_modules, name);
	end;


	
	-- Returns the list of preferred schematic libraries:
	function get_preferred_libraries_schematic (module : in pac_generic_modules.cursor)
		return et_meta.pac_preferred_libraries_schematic.list
	is begin
		return element (module).meta.schematic.preferred_libs;
	end get_preferred_libraries_schematic;

	-- Returns the list of preferred board libraries (non-electrical packages):
	function get_preferred_libraries_board (module : in pac_generic_modules.cursor)
		return et_meta.pac_preferred_libraries_board.list
	is begin
		return element (module).meta.board.preferred_libs;
	end get_preferred_libraries_board;


	

	function sheet_description (
		module	: in pac_generic_modules.cursor;
		sheet	: in type_sheet)
		return et_frames.type_schematic_description is
		use et_frames;

		use pac_schematic_descriptions;
		cursor : pac_schematic_descriptions.cursor;

		use et_schematic;

		procedure query_descriptions (
			module_name	: in type_module_name.bounded_string;
			module		: in type_module) is
		begin
			cursor := find (module.frames.descriptions, sheet);
		end query_descriptions;
										 
	begin -- sheet_description
		query_element (
			position	=> module,
			process		=> query_descriptions'access);
		
		if cursor /= pac_schematic_descriptions.no_element then
			return element (cursor);
		else
		-- If the sheet has no description, then return the defaults.
			return (others => <>);
		end if;
	end sheet_description;
	
	procedure port_not_at_edge (name : in et_general.type_net_name.bounded_string) is 
		use et_string_processing;
	begin
		log (ERROR, "port " & enclose_in_quotes (et_general.to_string (name)) &
			" must be at the edge of the submodule !", console => true);
		raise constraint_error;
	end;

	
	function port_connected (
	-- Returns true if given port of netchanger is connected with any net.
		module	: in pac_generic_modules.cursor;
		port	: in et_netlists.type_port_netchanger)
		return boolean is
		result : boolean := false; -- to be returned. goes true on the first (and only) match.

		use et_schematic;

		procedure query_nets (
			module_name	: in type_module_name.bounded_string;
			module		: in type_module) is
			use type_nets;
			net_cursor : type_nets.cursor := module.nets.first;

			procedure query_strands (
				net_name	: in et_general.type_net_name.bounded_string;
				net			: in type_net) is
				use type_strands;
				strand_cursor : type_strands.cursor := net.strands.first;

				procedure query_segments (strand : in type_strand) is
					use type_net_segments;
					segment_cursor : type_net_segments.cursor := strand.segments.first;

					procedure query_ports (segment : in type_net_segment) is 
						use et_submodules;

						use et_netlists;
						use type_ports_netchanger;
						port_cursor : type_ports_netchanger.cursor := segment.ports_netchangers.first;
					begin
						while port_cursor /= type_ports_netchanger.no_element loop
							if element (port_cursor) = port then
								result := true;
								exit; -- no more searching for netchanger ports required
							end if;
							next (port_cursor);
						end loop;
					end query_ports;
					
				begin -- query_segments
					while result = false and segment_cursor /= type_net_segments.no_element loop
						
						query_element (
							position	=> segment_cursor,
							process		=> query_ports'access);
						
						next (segment_cursor);
					end loop;
				end query_segments;
				
			begin -- query_strands
				while result = false and strand_cursor /= type_strands.no_element loop

					query_element (
						position	=> strand_cursor,
						process		=> query_segments'access);
					
					next (strand_cursor);
				end loop;
			end query_strands;
			
		begin -- query_nets
			while result = false and net_cursor /= type_nets.no_element loop

				type_nets.query_element (
					position	=> net_cursor,
					process		=> query_strands'access);

				next (net_cursor);
			end loop;
			
		end query_nets;
		
	begin -- port_not_connected

		pac_generic_modules.query_element (
			position	=> module,
			process		=> query_nets'access);
		
		return result;
	end port_connected;

	
	function netchanger_as_port_available (
	-- Returns true if the given net provides a netchanger that may serve as port
	-- to a parent module.
		module		: in pac_generic_modules.cursor;
		net			: in et_schematic.type_nets.cursor;
		direction	: in et_submodules.type_netchanger_port_name) -- master/slave 
		return boolean is
		
		result : boolean := false; -- to be returned. goes true on the first
		-- suitable netchanger found.

		use et_schematic;
		
		procedure query_strands (
			net_name	: in et_general.type_net_name.bounded_string;
			net			: in type_net) is
			use type_strands;
			strand_cursor : type_strands.cursor := net.strands.first;

			procedure query_segments (strand : in type_strand) is
				use type_net_segments;
				segment_cursor : type_net_segments.cursor := strand.segments.first;

				procedure query_ports (segment : in type_net_segment) is 
					use et_submodules;

					use et_netlists;
					use type_ports_netchanger;
					port_cursor : type_ports_netchanger.cursor := segment.ports_netchangers.first;
				begin
					while port_cursor /= type_ports_netchanger.no_element loop

						-- If the given direction is MASTER, then we must look for a SLAVE netchanger
						-- port (and vice versa) in the net segment.
						if element (port_cursor).port = opposide_port (direction) then 

							-- The opposide port must be not connected. In that case 
							-- suitable netchanger has been found:
							if not port_connected (
								module	=> module,
								port	=> (index	=> element (port_cursor).index,
											port	=> direction)) then
								
								result := true;
								exit; -- no more searching for netchanger ports required
							end if;

						end if;
						
						next (port_cursor);
					end loop;
				end query_ports;
				
			begin -- query_segments
				while result = false and segment_cursor /= type_net_segments.no_element loop
					
					query_element (
						position	=> segment_cursor,
						process		=> query_ports'access);
					
					next (segment_cursor);
				end loop;
			end query_segments;
			
		begin -- query_strands
			while result = false and strand_cursor /= type_strands.no_element loop

				query_element (
					position	=> strand_cursor,
					process		=> query_segments'access);
				
				next (strand_cursor);
			end loop;
		end query_strands;
		
	begin -- netchanger_as_port_available
		type_nets.query_element (
			position	=> net,
			process		=> query_strands'access);
		
		return result;
	end netchanger_as_port_available;

	procedure save_module (
		module_cursor	: in pac_generic_modules.cursor;
		save_as_name	: in type_module_name.bounded_string := to_module_name (""); -- motor_driver, templates/clock_generator							  
		log_threshold	: in et_string_processing.type_log_level) 
	is separate;

	function to_string (section : in type_section) return string is
	-- Converts a section like SEC_NET to a string "net".
		len : positive := type_section'image (section)'length;
	begin
		return to_lower (type_section'image (section) (5..len));
	end to_string;

	procedure read_module (
	-- Reads a module file and stores its content as generic module in container modules.
	-- The file name may contain environment variables.
	-- The file must exist, must be visible from the current working directory.
		file_name 		: in string; -- motor_driver.mod, templates/clock_generator.mod
		log_threshold	: in et_string_processing.type_log_level) 
		is separate;

	procedure create_module (
		module_name		: in type_module_name.bounded_string; -- motor_driver, templates/clock_generator
		log_threshold	: in et_string_processing.type_log_level) is

		module_cursor : pac_generic_modules.cursor;
		inserted : boolean;
		use et_string_processing;
	begin
		log (
			text	=> "creating module " & enclose_in_quotes (to_string (module_name)) & " ...",
			level	=> log_threshold);

		-- We create the new module only if does not exist already:

		-- Create an empty module named after the given module name.
		-- So the module names are things like "motor_driver", "templates/clock_generator".
	
		-- CS: make sure the module is inside the current project directory.
		
		pac_generic_modules.insert (
			container	=> generic_modules,
			key			=> module_name,
			position	=> module_cursor,
			inserted	=> inserted);

		if inserted then
			save_module (module_name, log_threshold + 1);
		else
			log (text => "module " & enclose_in_quotes (to_string (module_name)) &
					" already exists -> not created.", level => log_threshold + 1);
		end if;

	end create_module;

	procedure save_module (
		module_name		: in type_module_name.bounded_string; -- motor_driver, templates/clock_generator
		log_threshold	: in et_string_processing.type_log_level) is

		module_cursor : pac_generic_modules.cursor := locate_module (module_name);

		use et_string_processing;
		use ada.directories;

		file_name : constant string := to_string (module_name) &
			latin_1.full_stop & module_file_name_extension;
		-- motor_driver.mod or templates/clock_generator.mod
	begin
		log (
			text	=> "saving module " & enclose_in_quotes (to_string (module_name)) & " ...",
			level	=> log_threshold);

		-- We save the module if it exists:
		if module_cursor /= pac_generic_modules.no_element then
	
			-- Make sure the module file is inside the current project directory.
			-- If the file is outside the project, issue a warning and do not save.
			if inside_project_directory (to_string (module_name)) then
			
				save_module (
					module_cursor	=> module_cursor,
					log_threshold 	=> log_threshold + 1);

			else
				log (WARNING, "module " & enclose_in_quotes (to_string (module_name)) &
					 " is outside the project and will not be saved !");
			end if;
			
		else
			log (WARNING, "module " & enclose_in_quotes (to_string (module_name)) &
					" does not exist !");
		end if;
		
	end save_module;
	
	procedure delete_module (
		module_name		: in type_module_name.bounded_string; -- motor_driver, templates/clock_generator
		log_threshold	: in et_string_processing.type_log_level) is

		module_cursor : pac_generic_modules.cursor := locate_module (module_name);

		use et_string_processing;
		use ada.directories;

		file_name : constant string := append_extension (to_string (module_name));
		-- motor_driver.mod or templates/clock_generator.mod

		file_found : boolean := true;
		module_found : boolean := true;
	begin
		log (
			text	=> "deleting module " & enclose_in_quotes (to_string (module_name)) & " ...",
			level	=> log_threshold);

		-- Delete the module file in case it exists already:
		if exists (file_name) then
			delete_file (file_name);
		else
			file_found := false;
		end if;
		
		-- Remove the module from collection of generic modules:
		if module_cursor /= pac_generic_modules.no_element then
	
			---- Make sure the module file is inside the current project directory.
			---- If the file is outside the project, issue a warning and do not delete.
			--if inside_project_directory (to_string (module_name)) then
				
			pac_generic_modules.delete (
				container	=> generic_modules,
				position	=> module_cursor);

			--else
				--log (WARNING, "module " & enclose_in_quotes (to_string (module_name)) &
					 --" is outside the project and will not be deleted !");
			--end if;
		else
			module_found := false;
		end if;

		-- If neither the module nor the module file have been found, raise error:
		if not file_found and not module_found then
			raise semantic_error_1 with
				"ERROR: Neither module " & enclose_in_quotes (to_string (module_name)) 
				& " nor the file " & enclose_in_quotes (file_name) & " exist !";
		end if;
	end delete_module;



	function exists (
	-- Returns true if the given module provides the given port.
	-- The module being searched in must be in the rig already.
		module			: in et_submodules.type_submodules.cursor;
		port			: in et_general.type_net_name.bounded_string; -- clock_output
		direction		: in et_submodules.type_netchanger_port_name) -- master/slave
		return boolean is

		result : boolean := false; -- to be returned
		
		use et_string_processing;
		use et_submodules;
		use et_schematic;
		
		submodule_file : type_submodule_path.bounded_string; -- $ET_TEMPLATES/motor_driver.mod
		module_name : type_module_name.bounded_string; 
		module_cursor : pac_generic_modules.cursor;

		procedure query_nets (
			module_name	: in type_module_name.bounded_string;
			module		: in type_module) is
			net_cursor : type_nets.cursor;

			-- The port being inquired is a net inside the submodule.
			net : constant string := et_general.to_string (port);
			use type_nets;
		begin
			-- locate the net in the submodule
			net_cursor := find (module.nets, to_net_name (net));

			-- If net found, test its scope. If it is global,
			-- then all requirements are met -> result true.
			-- If net is local, then a netchanger is required.
			if net_cursor /= type_nets.no_element then -- net found

				case element (net_cursor).scope is
					when et_netlists.GLOBAL => 
						result := true;

					when et_netlists.LOCAL =>
						if netchanger_as_port_available (module_cursor, net_cursor, direction) then
							result := true;
						else
							result := false;
						end if;
				end case;
				
			else -- net not found: result is false
				result := false;
			end if;

		end query_nets;
		
	begin -- exists
		submodule_file := type_submodules.element (module).file;

		module_name := to_module_name (remove_extension (to_string (submodule_file)));
		module_cursor := locate_module (module_name);

		pac_generic_modules.query_element (
			position	=> module_cursor,
			process		=> query_nets'access);
		
		return result;

		exception
			when event: others =>
				log_indentation_reset;
				log (text => ada.exceptions.exception_information (event), console => true);
				raise;
		
	end exists;

	
	function exists (
	-- Returns true if the given module provides the given submodule instance.
	-- The module being searched in must be in the rig already.						
		module		: in pac_generic_modules.cursor; -- the parent module that contains the submodule instance
		instance	: in et_general.type_module_instance_name.bounded_string) -- OSC1
		return boolean is

		instance_found : boolean := false; -- to be returned

		procedure query_submodules (
			module_name	: in type_module_name.bounded_string;
			module		: in et_schematic.type_module) is
			use et_submodules.type_submodules;
		begin
			if contains (module.submods, instance) then
				instance_found := true;
			end if;
		end query_submodules;

	begin -- exists
		-- search in the parent module for the given submodule instance
		pac_generic_modules.query_element (
			position	=> module,
			process		=> query_submodules'access);

		return instance_found;
	end exists;

	function exists (
	-- Returns true if the given submodule instance provides the
	-- given assembly variant. The submodule instance is searched for
	-- in the parent module indicated by cursor "module".
	-- The module being searched in must be in the rig already.												
		module		: in pac_generic_modules.cursor; -- the parent module that contains the submodule instance
		instance	: in et_general.type_module_instance_name.bounded_string; -- OSC1
		variant		: in et_general.type_variant_name.bounded_string) -- low_cost				
		return boolean is

		variant_found : boolean := false; -- to be returned

		procedure query_submodules (
			module_name	: in type_module_name.bounded_string;
			module		: in et_schematic.type_module) is
			use et_submodules;
			use et_submodules.type_submodules;
			submod_instance_cursor : et_submodules.type_submodules.cursor;
			submod_path : type_submodule_path.bounded_string;
			submod_name	: type_module_name.bounded_string;
			submod_cursor : pac_generic_modules.cursor;

			procedure query_variants (
			-- Locates the given assembly variant in the submodule.
			-- Sets flag variant_found.
				submodule_name	: in type_module_name.bounded_string;
				submodule		: in et_schematic.type_module) is
				use et_assembly_variants;
			begin
				if et_assembly_variants.pac_variants.contains (submodule.variants, variant) then
					variant_found := true;
				end if;
			end query_variants;
				
		begin -- query_submodules
			-- locate the submodule instance by the given instance name
			submod_instance_cursor := find (module.submods, instance);

			-- get the file name of the submodule like $ET_TEMPLATES/motor_driver.mod
			submod_path :=  element (submod_instance_cursor).file;

			-- convert the submodule path to a submodule name
			submod_name := to_module_name (remove_extension (to_string (submod_path)));

			--et_string_processing.log (text => "submod name " & to_string (submod_name));

			-- get a cursor to the submodule file
			submod_cursor := locate_module (submod_name);

			-- locate the given variant in the submodule
			pac_generic_modules.query_element (
				position	=> submod_cursor,
				process		=> query_variants'access);

		end query_submodules;
		
	begin -- exists
		-- search in the parent module for the given submodule instance
		pac_generic_modules.query_element (
			position	=> module,
			process		=> query_submodules'access);

		return variant_found;
	end exists;

	function exists (
	-- Returns true if the given module provides the given assembly variant.
	-- If the variant is an empty string then it is about the default variant
	-- which is always provided. The return is true in that case.
		module		: in pac_generic_modules.cursor;
		variant		: in et_general.type_variant_name.bounded_string) -- low_cost
		return boolean is

		use et_assembly_variants.pac_variants;

		result : boolean := false; -- to be returned

		procedure query_variants (
			module_name	: in type_module_name.bounded_string;
			module		: in et_schematic.type_module) is
		begin
			result := contains (module.variants, variant);
		end;
		
	begin -- exists
		if et_general.type_variant_name.length (variant) = 0 then
			result := true;
		else
			
			pac_generic_modules.query_element (
				position	=> module,
				process		=> query_variants'access);

		end if;
					
		return result;
	end exists;

	function exists (
	-- Returns true if the given module and variant provides the given device.
	-- Assumptions: 
	-- - The module being searched in must be in the rig already.
	-- - The assembly variant must exist in the module.
	-- - The device must exist in the module.
		module	: in pac_generic_modules.cursor; -- the module like motor_driver
		variant	: in et_general.type_variant_name.bounded_string; -- low_cost				
		device	: in type_name)
		return boolean is

		result : boolean := false; -- to be returned

		procedure query_variants (
			module_name	: in type_module_name.bounded_string;
			module		: in et_schematic.type_module) is
			use et_assembly_variants.pac_variants;
			variant_cursor : et_assembly_variants.pac_variants.cursor;

			procedure query_devices (
				variant_name	: in et_general.type_variant_name.bounded_string;
				variant			: in et_assembly_variants.type_variant) is
				use et_assembly_variants;
				use et_assembly_variants.type_devices;
				device_cursor : et_assembly_variants.type_devices.cursor;
			begin
				device_cursor := find (variant.devices, device);

				-- The device may be listed in the assembly variant:
				if device_cursor /= et_assembly_variants.type_devices.no_element then
					case element (device_cursor).mounted is
						when YES => result := true; -- mounted with alternative value, partcode or purpose
						when NO  => result := false; -- not mounted
					end case;
				else
				-- The device may be NOT listed in the assembly variant. Means it is mounted always.
					result := true;
				end if;
					
			end query_devices;
				
		begin -- query_variants
			variant_cursor := find (module.variants, variant);

			query_element (
				position	=> variant_cursor,
				process		=> query_devices'access);
		end;
		
	begin -- exists
-- 		log (text => "module " & enclose_in_quotes (to_string (module_name)) &
-- 			" variant " & enclose_in_quotes (et_general.to_variant (variant)) &
-- 			" querying device " & to_string (device),
-- 			level => log_threshold);

		pac_generic_modules.query_element (
			position	=> module,
			process		=> query_variants'access);
		
		return result;
	end exists;

	function alternative_device (
	-- Returns a cursor to the alternative device in the given module
	-- and given assembly variant.
	-- Assumptions: 
	-- - The module being searched in must be in the rig already.
	-- - The assembly variant must exist in the module.
	-- - The device must exist in the module.
	-- - The device must have an entry in the given assembly variant,
	--   otherwise the return is no_element.
		module	: in pac_generic_modules.cursor; -- the module like motor_driver
		variant	: in et_general.type_variant_name.bounded_string; -- low_cost				
		device	: in type_name)
		return et_assembly_variants.type_devices.cursor is

		cursor : et_assembly_variants.type_devices.cursor; -- to be returned;
		
		procedure query_variants (
			module_name	: in type_module_name.bounded_string;
			module		: in et_schematic.type_module) is
			use et_assembly_variants.pac_variants;
			
			variant_cursor : et_assembly_variants.pac_variants.cursor;

			procedure query_devices (
				variant_name	: in et_general.type_variant_name.bounded_string;
				variant			: in et_assembly_variants.type_variant) is
				use et_assembly_variants.type_devices;
			begin
				cursor := find (variant.devices, device);
			end query_devices;
				
		begin -- query_variants
			variant_cursor := find (module.variants, variant);

			query_element (
				position	=> variant_cursor,
				process		=> query_devices'access);
		end;
		
	begin -- alternative_device

		pac_generic_modules.query_element (
			position	=> module,
			process		=> query_variants'access);
		
		return cursor;
	end alternative_device;

	function alternative_submodule (
	-- Returns a cursor to the alternative submodule variant in the given module
	-- and given assembly variant.
	-- Assumptions: 
	-- - The module being searched in must be in the rig already.
	-- - The assembly variant must exist in the module.
	-- - The suubmodule must have been instantiated in the module.
	-- - The submodule must have an entry in the given assembly variant,
	--   otherwise the return is no_element.
	-- If the given variant is an emtpy string (means default variant) the return
	-- is no_element.
		module	: in pac_generic_modules.cursor; -- the module like motor_driver
		variant	: in et_general.type_variant_name.bounded_string; -- low_cost				
		submod	: in et_general.type_module_instance_name.bounded_string) -- OSC1
		return et_assembly_variants.type_submodules.cursor is

		cursor : et_assembly_variants.type_submodules.cursor; -- to be returned;
		
		procedure query_variants (
			module_name	: in type_module_name.bounded_string;
			module		: in et_schematic.type_module) is
			use et_assembly_variants.pac_variants;

			variant_cursor : et_assembly_variants.pac_variants.cursor;

			procedure query_submodules (
				variant_name	: in et_general.type_variant_name.bounded_string;
				variant			: in et_assembly_variants.type_variant) is
				use et_assembly_variants.type_submodules;
			begin
				cursor := find (variant.submodules, submod);
			end query_submodules;
				
		begin -- query_variants
			variant_cursor := find (module.variants, variant);

			query_element (
				position	=> variant_cursor,
				process		=> query_submodules'access);
		end;
		
	begin -- alternative_submodule
		if et_general.is_default (variant) then
			cursor := et_assembly_variants.type_submodules.no_element;
		else
			pac_generic_modules.query_element (
				position	=> module,
				process		=> query_variants'access);
		end if;
		
		return cursor;
	end alternative_submodule;
	
	function deepest_conductor_layer (
		module	: in pac_generic_modules.cursor) -- the module like motor_driver
		return et_pcb_stack.type_signal_layer is
	begin
		return et_pcb_stack.deepest_layer (pac_generic_modules.element (module).board.stack);
	end deepest_conductor_layer;

	function layout_rules_assigned (
		module	: in pac_generic_modules.cursor) -- the module like motor_driver
		return boolean is

		use et_design_rules.pac_file_name;
	begin
		if length (pac_generic_modules.element (module).rules.layout) > 0 then
			return true;
		else
			return false;
		end if;
	end layout_rules_assigned;

	function get_pcb_design_rules (
		module	: in pac_generic_modules.cursor) -- the module like motor_driver
		return et_design_rules.type_design_rules -- JLP_ML4_standard.dru
	is
		use et_design_rules;
	begin
		return get_rules (element (module).rules.layout); 
	end get_pcb_design_rules;

	
end et_project.modules;
	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
