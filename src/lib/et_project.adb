------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                               PROJECT                                    --
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
with ada.characters.latin_1;	--use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;
with ada.strings; 				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.text_io;				use ada.text_io;

with ada.tags;

with ada.exceptions;
with ada.directories;
with gnat.directory_operations;

with ada.containers;            use ada.containers;
with ada.containers.ordered_maps;

with et_general;				use et_general;
with et_coordinates;
with et_string_processing;
with et_export;
with et_import;
with et_schematic;
with et_packages;
with et_pcb;
with et_pcb_stack;
with et_pcb_coordinates;
with et_conventions;
with submodules;
with assembly_variants;
with et_material;
with netlists;
with et_text;					use et_text;
with et_geometry;				use et_geometry;
with general_rw;				use general_rw;
with pcb_rw;					use pcb_rw;
with pcb_rw.device_packages;	use pcb_rw.device_packages;
with schematic_rw;				use schematic_rw;
with symbol_rw;					use symbol_rw;
with device_rw;					use device_rw;
with et_symbols;
with et_devices;				use et_devices;
with et_frames;
with frame_rw;
with et_meta;
with et_design_rules;

package body et_project is

	use et_general.type_net_name;

	
	function to_string (project_name : in type_project_name.bounded_string) return string is
	begin
		return type_project_name.to_string (project_name);
	end to_string;
	
	function to_project_name (name : in string) return type_project_name.bounded_string is
	-- Converts the given string to type_project_name.
	begin
		return type_project_name.to_bounded_string (name);
	end to_project_name;

	function to_string (path : in type_et_project_path.bounded_string) return string is begin
		return type_et_project_path.to_string (path);
	end to_string;

	function to_project_path (path : in string) return type_et_project_path.bounded_string is begin
		return type_et_project_path.to_bounded_string (path);
	end to_project_path;

	function to_module_file_name (name : in string) return type_module_file_name.bounded_string is begin
		return type_module_file_name.to_bounded_string (name);
	end;

	function to_string (name : in type_module_file_name.bounded_string) return string is begin
		return type_module_file_name.to_string (name);
	end;
	
	function exists (module : in type_module_name.bounded_string) return boolean is begin
	-- Returns true if the module with the given name exists in container modules.
		return type_modules.contains (modules, module);
	end;

	function locate_module (name : in type_module_name.bounded_string) -- motor_driver (without extension *.mod)
	-- Locates the given module in the global container "modules".
		return type_modules.cursor is
		use type_modules;
	begin
		return find (modules, name);
	end;

	function sheet_description (
		module	: in type_modules.cursor;
		sheet	: in type_sheet)
		return et_frames.type_schematic_description is
		use et_frames;

		use pac_schematic_descriptions;
		cursor : pac_schematic_descriptions.cursor;

		use et_schematic;
		use et_project.type_modules;

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
		module	: in type_modules.cursor;
		port	: in netlists.type_port_netchanger)
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
						use submodules;

						use netlists;
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

		type_modules.query_element (
			position	=> module,
			process		=> query_nets'access);
		
		return result;
	end port_connected;

	function locate_net (
	-- Returns a cursor to the given net in the given module.
		module_cursor	: in type_modules.cursor;
		net_name		: in type_net_name.bounded_string)
		return et_schematic.type_nets.cursor is
		use type_modules;
		use et_schematic.type_nets;
	begin
		return find (element (module_cursor).nets, net_name);
	end locate_net;

	
	
	function netchanger_as_port_available (
	-- Returns true if the given net provides a netchanger that may serve as port
	-- to a parent module.
		module		: in type_modules.cursor;
		net			: in et_schematic.type_nets.cursor;
		direction	: in submodules.type_netchanger_port_name) -- master/slave 
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
					use submodules;

					use netlists;
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
	
	function to_string (section : in type_section_name_rig_configuration) return string is
	-- Converts a section like SEC_MODULE_INSTANCES to a string "module_instances".
		len : positive := type_section_name_rig_configuration'image (section)'length;
	begin
		return to_lower (type_section_name_rig_configuration'image (section) (5..len));
	end to_string;

	function to_sheet_name_text_size (size : in string) return type_sheet_name_text_size is
	-- Converts a string to type_sheet_name_text_size.
	begin
		return type_sheet_name_text_size'value (size);
	end to_sheet_name_text_size;

	function to_file_name_text_size (size : in string) return type_file_name_text_size is
	-- Converts a string to type_file_name_text_size.
	begin
		return type_file_name_text_size'value (size);
	end to_file_name_text_size;

	function compare_connectors (left, right : in type_connector) return boolean is
	-- Returns true if left connector comes before right connector.
	-- Returns false if connectors are equal.
		use et_devices.type_purpose;
		use type_module_instance_name;
		r : boolean := false; -- to be returned
	begin
		-- First we compare instance_A
		if left.instance_A > right.instance_A then
			r := true;
		elsif left.instance_A < right.instance_A then
			r := false;
		else -- left instance_A equals right instance_A

			-- compare instance_B
			if left.instance_B > right.instance_B then
				r := true;
			elsif left.instance_B < right.instance_B then
				r := false;
			else -- left instance_B equals right instance_B

				-- compare purpose_A
				if left.purpose_A > right.purpose_A then
					r := true;
				elsif left.purpose_A < right.purpose_A then
					r := false;
				else -- left purpose_A equals right purpose_A

					-- compare purpose_B
					if left.purpose_B > right.purpose_B then
						r := true;
					elsif left.purpose_B < right.purpose_B then
						r := false;
					else 
						-- left purpose_B equals right purpose_B
						-- means: connectors are equal
						r := false;
					end if;
				end if;
			end if;
		end if;
		
		return r;
	end compare_connectors;
	
	procedure create_supplementary_directories (
		path			: in string;
		log_threshold	: in et_string_processing.type_log_level) is
		use et_string_processing;
		use ada.directories;
		use gnat.directory_operations;

		procedure create_library_subdirs (path : in string) is
		begin
			create_directory (compose (path, directory_libraries_devices));
			create_directory (compose (path, directory_libraries_symbols));
			create_directory (compose (path, directory_libraries_packages));			
			--log ("subdir " & compose (path, directory_libraries_devices));
		end create_library_subdirs;

		use et_export;
		
	begin -- create_supplementary_directories
		log (text => "creating subdirectories for supplementary stuff ...", level => log_threshold);
		create_directory (compose (path, directory_libraries));
		create_library_subdirs (compose (path, directory_libraries));
		
		--create_directory (compose (path, directory_dru));
		--create_directory (compose (path, directory_cam));
		--create_directory (compose (path, directory_net_classes));
		create_directory (compose (path, directory_templates));
		create_directory (compose (path, directory_export));
		make_dir (path & dir_separator & directory_export & dir_separator & directory_cam);
		make_dir (path & dir_separator & directory_export & dir_separator & directory_cam & dir_separator & directory_bom);
		make_dir (path & dir_separator & directory_export & dir_separator & directory_cam & dir_separator & directory_netlists);
		make_dir (path & dir_separator & directory_export & dir_separator & directory_cam & dir_separator & directory_pick_and_place);
		
		--create_directory (compose (path, directory_settings));
		create_directory (compose (path, directory_reports));
		create_directory (compose (path, directory_documentation));
		create_directory (compose (path, directory_miscellaneous));
	end create_supplementary_directories;

	procedure write_rig_configuration_header is 
		use et_general;
		use et_string_processing;
	begin
		-- write a nice header
		put_line (comment_mark & " " & system_name & " rig configuration file");
		put_line (comment_mark & " " & date);
		put_line (comment_mark & " " & row_separator_double);
		new_line;
	end;

	procedure write_rig_configuration_footer is
		use et_string_processing;
	begin
		-- write a nice footer
		new_line;
		put_line (comment_mark & " " & row_separator_double);
		put_line (comment_mark & " " & date);
		put_line (comment_mark & " rig configuration file end");
		new_line;
	end;
	
	procedure create_project_directory (
	-- Creates the given project directory in the given project_path.
	-- Creates a default rig configuration file.
	-- Already existing projects in given project_path are overwritten.
		project_name	: in type_project_name.bounded_string;		-- blood_sample_analyzer
		project_path	: in type_et_project_path.bounded_string; 	-- /home/user/et_projects
		log_threshold	: in et_string_processing.type_log_level) is
		use et_general;
		use ada.directories;
		use et_string_processing;
		use type_project_name;
		use type_et_project_path;

		package type_path is new generic_bounded_length (project_name_max + project_path_max + 1); -- incl. directory separator
		use type_path;
		path : type_path.bounded_string := to_bounded_string (compose (to_string (project_path), to_string (project_name)));

		procedure create_rig_configuration is
		-- create the rig configuration file
			file_handle : ada.text_io.file_type;
			rig_conf_file : type_rig_configuration_file_name.bounded_string; -- led_matrix.conf
		begin
			log (text => "creating the default rig configuration file ...", level => log_threshold + 1);

			-- compose the full file name			
			rig_conf_file := type_rig_configuration_file_name.to_bounded_string (compose (
				containing_directory	=> to_string (path),
				name 					=> to_string (project_name),
				extension 				=> rig_configuration_file_extension));

			-- create the file
			create (
				file => file_handle,
				mode => out_file, 
				name => type_rig_configuration_file_name.to_string (rig_conf_file));

			set_output (file_handle);

			write_rig_configuration_header;
			
			-- section module instances
			section_mark (section_module_instances, HEADER);

			section_mark (section_module, HEADER);			
			write (keyword => keyword_generic_name, parameters => to_string (project_name));
			write (keyword => keyword_instance_name, parameters => to_string (project_name));
			section_mark (section_module, FOOTER);			
			
			-- CS In the future, write other things here that characterize the instance.
			section_mark (section_module_instances, FOOTER);


			-- section connectors
			new_line;
			section_mark (section_module_connections, HEADER);

			section_mark (section_connector, HEADER);			
			write (keyword => comment_mark & " " & keyword_instance_A, parameters => to_string (project_name));
			write (keyword => comment_mark & " " & keyword_purpose_A, wrap => true, parameters => "power_in");
			new_line;
			write (keyword => comment_mark & " " & keyword_instance_B, parameters => "power_supply");
			write (keyword => comment_mark & " " & keyword_purpose_B, wrap => true, parameters => "power_out");
			new_line;
			write (keyword => comment_mark & " " & keyword_net_comparator, parameters => "on"); -- CS image of enum type
			write (keyword => comment_mark & " " & keyword_net_comparator_warn_only, parameters => "on"); -- CS image of enum type
			section_mark (section_connector, FOOTER);			
			
			-- CS In the future, write other things here that characterize the board to board connection
			section_mark (section_module_connections, FOOTER);

			-- close the file
			write_rig_configuration_footer;
			set_output (standard_output);
			close (file_handle);
			
		end create_rig_configuration;

	begin -- create_project_directory
		log (text => "creating native project " & enclose_in_quotes (to_string (path)) &
			 " ...", level => log_threshold);
		log_indentation_up;
		
		-- delete previous project directory
		if exists (to_string (path)) then
			delete_tree (to_string (path));
		end if;
		
		-- create project root directory
		create_path (to_string (path));
		
		create_supplementary_directories (to_string (path), log_threshold + 1);
		
		create_rig_configuration;

		log_indentation_down;
		
		exception when event:
			others => 
				log (text => ada.exceptions.exception_message (event), console => true);
				raise;
		
	end create_project_directory;

	procedure create_project_directory_bare (
	-- Creates a bare project (without a rig configuration file).
	-- Already existing projects in given path are overwritten.
		project_name	: in type_project_name.bounded_string;		-- blood_sample_analyzer
		project_path	: in type_et_project_path.bounded_string; 	-- /home/user/et_projects
		log_threshold	: in et_string_processing.type_log_level) is
		use et_general;
		use ada.directories;
		use et_string_processing;
		use type_project_name;
		use type_et_project_path;

		package type_path is new generic_bounded_length (project_name_max + project_path_max + 1); -- incl. directory separator
		use type_path;
		path : type_path.bounded_string := to_bounded_string (compose (to_string (project_path), to_string (project_name)));

		procedure create_library_subdirs (path : in string) is
		begin
			create_directory (compose (path, directory_libraries_devices));
			create_directory (compose (path, directory_libraries_symbols));
			create_directory (compose (path, directory_libraries_packages));			
		end create_library_subdirs;

	begin -- create_project_directory_bare
		log (text => "creating bare native project " & to_string (path) & " ...", level => log_threshold);
		log_indentation_up;
		
		-- delete previous project directory
		if exists (to_string (path)) then
			delete_tree (to_string (path));
		end if;
		
		-- create project root directory
		create_path (to_string (path));

		create_supplementary_directories (to_string (path), log_threshold + 1);

		log_indentation_down;
		
		exception when event:
			others => 
				log (text => ada.exceptions.exception_message (event), console => true);
				raise;
		
	end create_project_directory_bare;

	procedure save_rig_configuration (
		project_name	: in type_project_name.bounded_string;		-- blood_sample_analyzer
		rig_conf_name	: in type_rig_configuration_file_name.bounded_string; -- demo, low_cost, fully_equipped
		rig				: in type_rig; -- the actual rig configuration				
		project_path	: in type_et_project_path.bounded_string; 	-- /home/user/et_projects
		log_threshold 	: in et_string_processing.type_log_level) is
	-- Saves the rig configuration in the file with the given name rig_conf_file.

		use ada.directories;
		use et_string_processing;
		use type_module_instances;
		use type_module_connectors;
		
		file_name : type_rig_configuration_file_name.bounded_string; -- the final full file name
		file_handle : ada.text_io.file_type;

		package type_path is new generic_bounded_length (project_name_max + project_path_max + 1); -- incl. directory separator
		use type_path;
		path : type_path.bounded_string := to_bounded_string (compose (to_string (project_path), to_string (project_name)));

		procedure query_instance (instance_cursor : in type_module_instances.cursor) is
		begin
			section_mark (section_module, HEADER);			
			write (keyword => keyword_generic_name, parameters => to_string (element (instance_cursor).generic_name));
			write (keyword => keyword_instance_name, parameters => et_general.to_string (key (instance_cursor)));
			section_mark (section_module, FOOTER);
		end;

		procedure query_connections (connection_cursor : in type_module_connectors.cursor) is
			con : type_connector := element (connection_cursor);
		begin
			section_mark (section_connector, HEADER);
			write (keyword => keyword_instance_A, parameters => to_string (con.instance_A));
			write (keyword => keyword_purpose_A, wrap => true, parameters => et_devices.to_string (con.purpose_A));
			new_line;
			write (keyword => keyword_instance_B, parameters => to_string (con.instance_B));
			write (keyword => keyword_purpose_B, wrap => true, parameters => et_devices.to_string (con.purpose_B));

			-- CS: net comparator, warnings
			
			section_mark (section_connector, FOOTER);
		end;
		
	begin -- save_rig_configuration
		log (text => "saving rig configuration ...", level => log_threshold);
		reset_tab_depth;
		log_indentation_up;

		-- compose the full file name
		file_name := type_rig_configuration_file_name.to_bounded_string (compose (
			containing_directory	=> to_string (path),
			name 					=> to_string (rig_conf_name),
			extension 				=> rig_configuration_file_extension));

		-- create the file
		create (
			file => file_handle,
			mode => out_file, 
			name => to_string (file_name));
		
		set_output (file_handle);
		write_rig_configuration_header;		

		-- section module instances
		section_mark (section_module_instances, HEADER);
		iterate (rig.module_instances, query_instance'access);
		-- CS In the future, write other things here that characterize the instance.
		section_mark (section_module_instances, FOOTER);

		-- section connectors
		new_line;
		section_mark (section_module_connections, HEADER);
		iterate (rig.connections, query_connections'access);
		-- CS In the future, write other things here that characterize the board to board connection
		section_mark (section_module_connections, FOOTER);

		-- close the file
		write_rig_configuration_footer;
		set_output (standard_output);
		close (file_handle);
		
		log_indentation_down;

		exception when event:
			others => 
				log (text => ada.exceptions.exception_message (event), console => true);
				close (file_handle);
				raise;

	end save_rig_configuration;

	procedure save_module (
	-- Saves the given generic module in the given file.
		module_cursor		: in type_modules.cursor;
		module_file_name	: in type_module_file_name.bounded_string; -- led_matrix.mod
		log_threshold		: in et_string_processing.type_log_level) 
		is separate;
	
	procedure save_module (
		module_cursor	: in type_modules.cursor;					-- the module
		project_name	: in type_project_name.bounded_string;		-- blood_sample_analyzer
		module_name		: in type_module_name.bounded_string := to_module_name ("");	-- motor_driver
		project_path	: in type_et_project_path.bounded_string; 	-- /home/user/et_projects
		log_threshold 	: in et_string_processing.type_log_level) is
	-- Saves the given module in the module file of the given project.
	-- If module_name not provided, the module will be named after the given project_name.
	-- CS: improve log messages !!
		
		use et_string_processing;
		use et_schematic;
		
		function make_module_file_name return type_module_file_name.bounded_string is 
		-- Creates the module/submodule file and writes a nice header in it.
			use ada.directories;
			use gnat.directory_operations;
			use type_project_name;
			use type_et_project_path;
			use et_general;

			package type_path is new generic_bounded_length (project_name_max + project_path_max + 1); -- incl. directory separator
			use type_path;
			path : type_path.bounded_string := to_bounded_string (compose (to_string (project_path), to_string (project_name)));

			module_file_name : type_module_file_name.bounded_string; -- led_matrix -- to be returned
			
		begin -- write_module_header
			log (text => "setting module file name ...", level => log_threshold + 1);

			-- If given module_name is empty (means it has not been passed), the module is named after the project.
			-- Otherwise the module name is as given by module_name.
			if type_module_name.length (module_name) = 0 then
				module_file_name := type_module_file_name.to_bounded_string (compose (
					containing_directory	=> to_string (path),
					name 					=> to_string (project_name),
					extension 				=> module_file_name_extension));
			else
				-- Compose the full path of the module file. 
				-- NOTE: The function "compose" in ada.directories does not work here
				-- because it does not accept directory separators in a module_name.
				module_file_name := to_module_file_name (
					to_string (path) &			-- ./project_abc
					dir_separator &				-- /
					to_string (module_name) &	-- motor_driver, templates/clock_generator
					latin_1.full_stop &			-- .
					module_file_name_extension	-- mod
					);
			end if;
			
			log (text => " module file name is now " & type_module_file_name.to_string (module_file_name), level => log_threshold + 2);

			return module_file_name;
		end make_module_file_name;
		
	begin -- save_module
		log (text => "saving module ...", level => log_threshold);
		reset_tab_depth;
		log_indentation_up;

		save_module (module_cursor, make_module_file_name, log_threshold);
	
		log_indentation_down;

		exception when event:
			others => 
				log (text => ada.exceptions.exception_message (event), console => true);
				raise;

	end save_module;

	

	

	function to_string (section : in type_section) return string is
	-- Converts a section like SEC_NET to a string "net".
		len : positive := type_section'image (section)'length;
	begin
		return to_lower (type_section'image (section) (5..len));
	end to_string;

	procedure read_module_file (
	-- Reads a module file and stores its content as generic module in container modules.
	-- The file name may contain environment variables.
	-- The file must exist, must be visible from the current working directory.
		file_name 		: in string; -- motor_driver.mod, templates/clock_generator.mod
		log_threshold	: in et_string_processing.type_log_level) 
		is separate;

	procedure create_module (
	-- Creates an empty generic module in container modules.								   
		module_name		: in type_module_name.bounded_string; -- motor_driver, templates/clock_generator
		log_threshold	: in et_string_processing.type_log_level) is

		use type_modules;
		module_cursor : type_modules.cursor;
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
		
		type_modules.insert (
			container	=> modules,
			key			=> module_name,
			position	=> module_cursor,
			inserted	=> inserted);

		if not inserted then
			log (text => "module " & enclose_in_quotes (to_string (module_name)) &
					" already exists -> not created.", level => log_threshold + 1);
		end if;

	end create_module;

	procedure save_module (
	-- Saves a generic module (from container modules) in a file inside the current project directory. 
		module_name		: in type_module_name.bounded_string; -- motor_driver, templates/clock_generator
		log_threshold	: in et_string_processing.type_log_level) is

		use type_modules;
		module_cursor : type_modules.cursor := locate_module (module_name);

		use et_string_processing;
		use ada.directories;

		file_name : constant string := to_string (module_name) & latin_1.full_stop & module_file_name_extension;
		-- motor_driver.mod or templates/clock_generator.mod
	begin
		log (
			text	=> "saving module " & enclose_in_quotes (to_string (module_name)) & " ...",
			level	=> log_threshold);

		-- We save the module only if it exists:
		if module_cursor /= type_modules.no_element then
	
			-- CS: make sure the module is inside the current project directory.
			-- This test is probably not required since module cursor points to a module 
			-- inside the project anyway. Module names are like file paths like "templates/motor_driver".

			save_module (
				module_cursor		=> module_cursor,					-- the module
				module_file_name	=> to_module_file_name (file_name),	-- blood_sample_analyzer
				log_threshold 		=> log_threshold + 1);
			
		else
			log (text => "module " & enclose_in_quotes (to_string (module_name)) &
					" does not exist !", level => log_threshold + 1);
		end if;
		
	end save_module;
	
	procedure delete_module (
	-- Deletes a generic module in container modules. 
	-- Deletes the module file of the generic module.
		module_name		: in type_module_name.bounded_string; -- motor_driver, templates/clock_generator
		log_threshold	: in et_string_processing.type_log_level) is

		use type_modules;
		module_cursor : type_modules.cursor := locate_module (module_name);

		use et_string_processing;
		use ada.directories;

		file_name : constant string := to_string (module_name) & latin_1.full_stop & module_file_name_extension;
		-- motor_driver.mod or templates/clock_generator.mod
	begin
		log (
			text	=> "deleting module " & enclose_in_quotes (to_string (module_name)) & " ...",
			level	=> log_threshold);

		-- We delete the module only if it exists:
		if module_cursor /= type_modules.no_element then
	
			-- CS: make sure the module is inside the current project directory.
			-- This test is probably not required since module cursor points to a module 
			-- inside the project anyway. Module names are like file paths like "templates/motor_driver".
			
			type_modules.delete (
				container	=> modules,
				position	=> module_cursor);
			
		else
			log (text => "module " & enclose_in_quotes (to_string (module_name)) &
					" does not exist !", level => log_threshold + 1);
		end if;

		-- Delete the module file in case it exists already:
		if exists (file_name) then
			delete_file (file_name);
		end if;
		
	end delete_module;
	
	procedure open_project (
		project_name 	: in type_project_name.bounded_string; -- blood_sample_analyzer
		log_threshold 	: in et_string_processing.type_log_level) is
	-- Enters the project directory specified by project_name.
	-- Searches for rig configuration files (*.conf), reads them and stores configurations in et_project.rigs.
	-- Searches for module files (*.mod), reads them and stores modules in et_project.modules.
		use et_string_processing;
		use ada.directories;

		-- We need a backup of the current working directory. When this procedure finishes,
		-- the working directory must restored.
		current_working_directory : string := current_directory;

	-- MODULES
		
		-- The search of rig module files requires this stuff:
		module_file_search : search_type; -- the state of the search
		module_file_filter : filter_type := (ordinary_file => true, others => false);

		procedure read_module_file_pre (module_file_handle : in directory_entry_type) is 
			file_name : string := simple_name (module_file_handle); -- motor_driver.mod
		begin
			read_module_file (file_name, log_threshold + 1);
		end;
		

	-- RIG CONFIGURATION		
		
		-- The search of rig configuration files requires this stuff:
		conf_file_search : search_type; -- the state of the search
		conf_file_filter : filter_type := (ordinary_file => true, others => false);

		procedure read_conf_file (conf_file_handle : in directory_entry_type) is 
			file_handle : ada.text_io.file_type;
			file_name : string := simple_name (conf_file_handle); -- my_rig_configuration.conf
			rig_cursor : type_rigs.cursor;
			rig_inserted : boolean;
			
			line : et_string_processing.type_fields_of_line;

			-- This is the section stack of the configuration file. 
			-- Here we track the sections. On entering a section, its name is
			-- pushed onto the stack. When leaving a section the latest section name is popped.
			max_section_depth : constant positive := 3;
			package stack is new general_rw.stack_lifo (
				item	=> type_section_name_rig_configuration,
				max 	=> max_section_depth);

			-- VARIABLES FOR TEMPORARILY STORAGE AND ASSOCIATED HOUSEKEEPING SUBPROGRAMS:
			generic_name : type_module_name.bounded_string; -- motor_driver
			instance_name : type_module_instance_name.bounded_string; -- DRV_1
			assembly_variant : et_general.type_variant_name.bounded_string; -- low_cost

			procedure clear_module_instance is begin
				generic_name := to_module_name ("");
				instance_name := to_instance_name ("");
			end clear_module_instance;
			
			purpose_A, purpose_B : et_devices.type_purpose.bounded_string; -- power_in, power_out
			instance_A, instance_B : type_module_instance_name.bounded_string; -- DRV_1, PWR

			procedure clear_connector is begin
				purpose_A := et_devices.type_purpose.to_bounded_string ("");
				purpose_A := purpose_B;
				instance_A := to_instance_name ("");
				instance_B := instance_A;
			end clear_connector;
			
			procedure process_line is

				procedure execute_section is
				-- Once a section concludes, the temporarily variables are read, evaluated
				-- and finally assembled to actual objects:

					procedure create_instance (
						rig_name	: in type_rig_configuration_file_name.bounded_string;
						rig			: in out type_rig) is
						instance_created : boolean;
						instance_cursor : type_module_instances.cursor;
					begin
						-- CS: test length of generic name and instance name. must be greater zero

						-- create an instanciated module in the rig
						rig.module_instances.insert (
							key			=> instance_name,
							new_item	=> (generic_name, assembly_variant),
							inserted	=> instance_created,
							position	=> instance_cursor
							);

						-- An instance may exist only once:
						if not instance_created then
							log (ERROR, "module instance '" 
								& to_string (instance_name) & "' already exists !", console => true);
							raise constraint_error;
						end if;

						clear_module_instance; -- clean up for next module instance
					end create_instance;

					procedure create_connection (
						rig_name	: in type_rig_configuration_file_name.bounded_string;
						rig			: in out type_rig) is
						connection_inserted : boolean;
						connection_cursor : type_module_connectors.cursor;
						use et_devices.type_purpose;
						use et_general.type_module_instance_name;
					begin
						-- If NONE of the four elements that make a module connection is specified,
						-- then do nothing. Otherwise ALL of them must be specified.
						if length (instance_A) = 0 and length (instance_B) = 0
							and length (purpose_A) = 0 and length (purpose_B) = 0 then
								null;
						else
							-- If ALL four elements are
							-- specified (means each of them contains something) then do further
							-- checks and create the connection.
							if length (instance_A) > 0 and length (instance_B) > 0
								and length (purpose_A) > 0 and length (purpose_B) > 0 then
								
								-- create a module connector in the rig
								rig.connections.insert (
									new_item	=> (
										instance_A	=> instance_A,
										instance_B	=> instance_B,
										purpose_A	=> purpose_A,
										purpose_B	=> purpose_B),
									inserted	=> connection_inserted,
									position	=> connection_cursor);

								-- A module connection may exist only once:
								if not connection_inserted then
									log (ERROR, "module connection already exists !", console => true);
									raise constraint_error;
								end if;

								clear_connector; -- clean up for next module connector

							-- If one of the four elements is not specified, output error message:
							else
								-- test length of instance_A/B and purpose A/B. must be greater zero
								if length (instance_A) = 0 then
									log (ERROR, "instance A not specified !", console => true);
									raise constraint_error;
								end if;

								if length (purpose_A) = 0 then
									log (ERROR, "purpose A not specified !", console => true);
									raise constraint_error;
								end if;						

								if length (instance_B) = 0 then
									log (ERROR, "instance B not specified !", console => true);
									raise constraint_error;
								end if;

								if length (purpose_B) = 0 then
									log (ERROR, "purpose B not specified !", console => true);
									raise constraint_error;
								end if;						
							end if;
							
						end if;
					end create_connection;
					
				begin -- execute_section
					case stack.current is
													
						when SEC_MODULE =>
							case stack.parent is
								when SEC_MODULE_INSTANCES =>	

									-- create an instanciated module in the rig
									type_rigs.update_element (
										container	=> rigs,
										position	=> rig_cursor,
										process		=> create_instance'access);
									
								when others => invalid_section;
							end case;

						when SEC_CONNECTOR =>
							case stack.parent is
								when SEC_MODULE_CONNECTIONS =>
									
									-- create a module connector in the rig
									type_rigs.update_element (
										container	=> rigs,
										position	=> rig_cursor,
										process		=> create_connection'access);
									
								when others => invalid_section;
							end case;

						when others => null; -- CS
					end case;
							
				end execute_section;
				
				function set (
				-- Tests if the current line is a section header or footer. Returns true in both cases.
				-- Returns false if the current line is neither a section header or footer.
				-- If it is a header, the section name is pushed onto the sections stack.
				-- If it is a footer, the latest section name is popped from the stack.
					section_keyword	: in string; -- [MODULE_INSTANCES
					section			: in type_section_name_rig_configuration) -- SEC_MODULE_INSTANCES
					return boolean is 
				begin -- set
					if f (line, 1) = section_keyword then -- section name detected in field 1
						if f (line, 2) = section_begin then -- section header detected in field 2
							stack.push (section);
							log (text => write_enter_section & to_string (section), level => log_threshold + 7);
							return true;
							
						elsif f (line, 2) = section_end then -- section footer detected in field 2

							-- The section name in the footer must match the name
							-- of the current section. Otherwise abort.
							if section /= stack.current then
								log_indentation_reset;
								invalid_section;
							end if;
							
							-- Now that the section ends, the data collected in temporarily
							-- variables is processed.
							execute_section;
							
							stack.pop;
							if stack.empty then
								log (text => write_top_level_reached, level => log_threshold + 7);
							else
								log (text => write_return_to_section & to_string (stack.current), level => log_threshold + 7);
							end if;
							return true;
							
						else
							log (ERROR, write_missing_begin_end, console => true);
							raise constraint_error;
						end if;
						
					else -- neither a section header nor footer
						return false;
					end if;
				end set;
				
			begin -- process_line
				if set (section_module_instances, SEC_MODULE_INSTANCES) then null;
				elsif set (section_module, SEC_MODULE) then null;
				elsif set (section_module_connections, SEC_MODULE_CONNECTIONS) then null;
				elsif set (section_connector, SEC_CONNECTOR) then null;
				else
					-- The line contains something else -> the payload data. 
					-- Temporarily this data is to be stored in corresponding variables.

					log (text => "line --> " & to_string (line), level => log_threshold + 7);
					
					case stack.current is

						when SEC_MODULE_INSTANCES =>
							case stack.parent is
								when SEC_INIT => null; -- nothing to do
								when others => invalid_section;
							end case;
							
						when SEC_MODULE_CONNECTIONS =>
							case stack.parent is
								when SEC_INIT => null; -- nothing to do
								when others => invalid_section;
							end case;

						when SEC_MODULE =>
							case stack.parent is
								when SEC_MODULE_INSTANCES =>							
									declare
										kw : string := f (line, 1);
										module_cursor : type_modules.cursor;
									begin
										if kw = keyword_generic_name then
											expect_field_count (line, 2);

											-- The generic name does not use the *.mod extension.
											generic_name := type_module_name.to_bounded_string (f (line,2));
											
											-- test whether a module with this generic name exists
											if not exists (generic_name) then
												log (ERROR, "module " & enclose_in_quotes (to_string (generic_name)) &
													 " does not exist !", console => true);
												raise constraint_error;
											end if;
											
										elsif kw = keyword_instance_name then
											expect_field_count (line, 2);
											instance_name := to_instance_name (f (line,2));

										elsif kw = keyword_assembly_variant then
											expect_field_count (line, 2);
											assembly_variant := et_general.to_variant (f (line,2));

											-- test whether module provides the assembly variant
											module_cursor := locate_module (generic_name);
											if not exists (module_cursor, assembly_variant) then
												log (ERROR, "module " & enclose_in_quotes (to_string (generic_name)) &
													 " does not provide assembly variant " &
													 enclose_in_quotes (et_general.to_variant (assembly_variant)) & " !",
													console => true);
												raise constraint_error;
											end if;
										else
											invalid_keyword (kw);
										end if;
									end;
									
								when others => invalid_section;
							end case;

						when SEC_CONNECTOR =>							
							case stack.parent is
								when SEC_MODULE_CONNECTIONS =>
									declare
										use et_devices;
										kw : string := f (line, 1);
									begin
										if kw = keyword_instance_A then
											expect_field_count (line, 2);
											instance_A := to_instance_name (f (line,2));
											-- CS: test if instance exists
										elsif kw = keyword_instance_B then
											expect_field_count (line, 2);
											instance_B := to_instance_name (f (line,2));
											-- CS: test if instance exists
											
										elsif kw = keyword_purpose_A then
											expect_field_count (line, 2);
											purpose_A := to_purpose (f (line,2));
											-- CS: test if a connector with this purpose exists in the instance
											
										elsif kw = keyword_purpose_B then
											expect_field_count (line, 2);
											purpose_B := to_purpose (f (line,2));
											-- CS: test if a connector with this purpose exists in the instance
											
										-- CS: net comparator and warning on/off
										else
											invalid_keyword (kw);
										end if;
									end;
									
								when others => invalid_section;
							end case;

						when others => null; -- CS
					end case;
				end if;

				exception when event: others =>
					log (text => "file " & file_name & space & affected_line (line) 
						 & to_string (line), console => true);
					raise;
				
			end process_line;
			
		begin -- read_conf_file
			-- write name of configuration file
			log (text => file_name, level => log_threshold + 1);
			log_indentation_up;

			-- open rig configuration file
			open (
				file => file_handle,
				mode => in_file, 
				name => file_name); -- demo.conf, low_cost.conf, fully_equipped.conf

			set_input (file_handle);

			-- Init section stack.
			stack.init;
			stack.push (SEC_INIT);

			-- create an empty rig - named after the given configuration file but without extension
			type_rigs.insert (
				container	=> rigs,
				key			=> to_bounded_string (base_name (file_name)), -- demo, low_cost, fully_equipped
				inserted	=> rig_inserted, -- should always be true
				position	=> rig_cursor);
			
			-- read the file line by line
			while not end_of_file loop
				line := et_string_processing.read_line (
					line 			=> get_line,
					number			=> ada.text_io.line (current_input),
					comment_mark 	=> comment_mark, -- comments start with "--"
					delimiter_wrap	=> true, -- strings are enclosed in quotations
					ifs 			=> space); -- fields are separated by space

				-- we are interested in lines that contain something. emtpy lines are skipped:
				if field_count (line) > 0 then
					process_line;
				end if;
			end loop;

			-- As a safety measure the top section must be reached:
			if stack.depth > 1 then 
				log (WARNING, write_section_stack_not_empty);
			end if;

			log_indentation_down;
			set_input (current_input);
			close (file_handle);

			exception when event: others =>
				if is_open (file_handle) then close (file_handle); end if;
				raise;
			
		end read_conf_file;
		
	begin -- open_project
		log (text => row_separator_double, level => log_threshold);
		log (text => "opening project " & to_string (project_name) & " ...", level => log_threshold, console => true);
		log_indentation_up;
		
		-- If the given project directory exists, enter it. Otherwise error message and abort.
		if exists (to_string (project_name)) then
			
			-- enter the project directory
			set_directory (to_string (project_name));

			--log (text => "current dir " & current_directory, level => log_threshold + 1);

			-- CS: It requires discussion whether loading all modules files at this time is reasonable.
			-- Even if a module will not be used it is going to be loaded. This causes more log information than required.
			-- A solution could be to load a module on reading the rig configuration file. The drawback is that the user
			-- would be required to setup a rig configuration even if she wants to design only one board.
			log (text => "looking for module files ...", level => log_threshold + 1);
			log_indentation_up;
			start_search (module_file_search, current_directory, module_file_name_extension_asterisk, module_file_filter);
			if more_entries (module_file_search) then
				search (current_directory, module_file_name_extension_asterisk, module_file_filter, read_module_file_pre'access);
			else
				log (WARNING, "No modules found !"); -- CS: write implications !
			end if;
			end_search (module_file_search);
			log_indentation_down;
			
			log (text => "looking for rig configuration files ...", level => log_threshold + 1);
			log_indentation_up;
			start_search (conf_file_search, current_directory, rig_configuration_file_extension_asterisk, conf_file_filter);
			if more_entries (conf_file_search) then
				search (current_directory, rig_configuration_file_extension_asterisk, conf_file_filter, read_conf_file'access);
			else
				log (WARNING, "No rig configuration files found !"); -- CS: write implications !
			end if;
			end_search (conf_file_search);
			log_indentation_down;
			
		else -- project directory does not exist
			log (ERROR, "Native project " & to_string (project_name) 
				 & " does not exist !", console => true);
			--log (text => "Example to open the native project by specifying the project directory:", console => true);			log ("Example to open the native project by specifying the project directory:", console => true);
			--log (system_name_cmd_line & "openetample to open the native project by specifying the project directory:", console => true);
			raise constraint_error;
		end if;

		log_indentation_down;

		-- Restore working directory.
		set_directory (current_working_directory);
		
		exception when event:
			others => 
				-- Restore working directory.
				set_directory (current_working_directory);

				raise;

	end open_project;

	procedure save_libraries (
	-- Saves the library containers (et_libraries.devices and et_packages.packages) in
	-- the directory specified by project_path and project_name.
		project_name	: in et_project.type_project_name.bounded_string;		-- blood_sample_analyzer
		project_path	: in et_project.type_et_project_path.bounded_string; 	-- /home/user/ecad
		log_threshold	: in et_string_processing.type_log_level) is
		use et_project;
		use type_project_name;
		use type_et_project_path;
		use ada.directories;
		use et_string_processing;

		package type_path is new generic_bounded_length (project_name_max + project_path_max + 1); -- incl. directory separator
		use type_path;
		path : type_path.bounded_string := to_bounded_string (
				compose (type_et_project_path.to_string (project_path), type_project_name.to_string (project_name)));
		-- Path now contains something like /home/user/ecad/blood_sample_analyzer
		
		use et_devices;

		procedure save_device (device_cursor : in type_devices.cursor) is 
			use type_devices;
		begin
			save_device (
				-- library name like: 
				-- /home/user/ecad/blood_sample_analyzer/libraries/devices/bel_connector_and_jumper_FEMALE_01X06.dev
				file_name		=> to_file_name 
					(
					to_string (path) & gnat.directory_operations.dir_separator & to_string (key (device_cursor))
					),

				-- the device model itself:
				device			=> element (device_cursor),
				log_threshold	=> log_threshold + 1); 
		end save_device;

		use et_packages;
		
		procedure save_package (package_cursor : in type_packages.cursor) is
			use type_package_model_file;
			use type_packages;
		begin
			save_package (
				-- package name like: 
				-- /home/user/ecad/blood_sample_analyzer/libraries/packages/bel_connector_and_jumper_FEMALE_01X06.pac
				file_name		=> to_file_name (
					to_string (path) & gnat.directory_operations.dir_separator &
					type_package_model_file.to_string (key (package_cursor))),

				-- the package model itself:
				packge			=> element (package_cursor),
				log_threshold	=> log_threshold + 1); 
		end save_package;
		
	begin -- save_libraries
		log (text => "saving libraries ...", level => log_threshold);
		log_indentation_up;

		log (text => "devices ...", level => log_threshold + 1);
		log_indentation_up;
		type_devices.iterate (devices, save_device'access);
		log_indentation_down;
		
		log (text => "packages ...", level => log_threshold + 1);
		log_indentation_up;
		type_packages.iterate (packages, save_package'access);
		log_indentation_down;

		log_indentation_down;			
	end save_libraries;

	procedure save_project (
		destination		: in type_project_name.bounded_string; -- /home/user/ecad/blood_sample_analyzer
		log_threshold 	: in et_string_processing.type_log_level) is
		use et_string_processing;
		use ada.directories;
		use type_modules;
		
		-- break down destination into path and project name:
		path : type_et_project_path.bounded_string := to_project_path (containing_directory (to_string (destination)));
		name : type_project_name.bounded_string := to_project_name (simple_name (to_string (destination)));

		procedure query_modules (module_cursor : in type_modules.cursor) is
		-- Saves a module or a submodule (indicated by module_cursor).
			module_name : type_module_name.bounded_string := key (module_cursor); -- motor_driver

			function in_project_directory return boolean is
			-- Tests whether the current module is inside the project directory.
			-- NOTE: This works on Linux only.
			-- 1. The expanded module_name may be a relative path to a directory outside the project.
			--    In this case the expanded path starts with ../ and the return will be false.
			-- 2. The expanded module_name may be a relative path to a subdirectory inside the project.
			--    In this case the return would be true.
			-- 3. The expanded module_name may be an absolute path pointing elsewhere in the filesystem.
			--    In this case the expanded path starts with / and the return will be false.
				use gnat.directory_operations;
				expanded_name : constant string := expand (to_string (module_name));
			begin
				if 	index (expanded_name, to_set (dir_separator)) = 1 or -- absolute path
					index (expanded_name, ".." & dir_separator) = 1 then -- relative outside the project
					return false;
				else
					return true;
				end if;
			end;
			
		begin -- query_modules
			log_indentation_up;

			-- Only those modules inside the project will be saved in the new project:
			if in_project_directory then
				log (text => "saving module " & to_string (module_name), level => log_threshold + 1);
				
				log_indentation_up;
				
				save_module (
					module_cursor	=> module_cursor, -- the module it is about
					project_name	=> name, -- blood_sample_analyzer
					module_name		=> module_name,	-- motor_driver
					project_path	=> path, -- /home/user/ecad
					log_threshold 	=> log_threshold + 2);

				-- FOR TESTING ONLY
				-- save libraries (et_libraries.devices and et_pcb.packages)
	-- 			save_libraries (
	-- 				project_name	=> name, -- blood_sample_analyzer
	-- 				project_path	=> path, -- /home/user/ecad
	-- 				log_threshold 	=> log_threshold + 1);
				
				log_indentation_down;
			end if;
			
			log_indentation_down;			
		end query_modules;

		procedure query_rig_configuration (rig_cursor : in type_rigs.cursor) is
			use type_rigs;
			rig_name : type_rig_configuration_file_name.bounded_string := key (rig_cursor);
		begin
			log_indentation_up;
			log (text => "rig configuration " & to_string (rig_name), level => log_threshold + 1);
			log_indentation_up;
			
			save_rig_configuration (
				project_name	=> name, -- blood_sample_analyzer
				rig_conf_name	=> rig_name, -- demo, low_cost, fully_equipped
				rig				=> element (rig_cursor), -- the actual rig configuration
				project_path	=> path,	-- /home/user/ecad
				log_threshold 	=> log_threshold + 1);
			
			log_indentation_down;
			log_indentation_down;
		end query_rig_configuration;
		
	begin -- save_project
		log (text => row_separator_double, level => log_threshold);
		log (text => "saving project as " & to_string (destination) & " ...", level => log_threshold, console => true);
		log_indentation_up;

		log (text => "path " & to_string (path));
		log (text => "name " & to_string (name));
		
		create_project_directory_bare (
			project_name	=> name, -- blood_sample_analyzer
			project_path	=> path, -- /home/user/ecad
			log_threshold 	=> log_threshold + 2);

		-- save modules
		iterate (modules, query_modules'access);

		-- save rig configuration files
		type_rigs.iterate (rigs, query_rig_configuration'access);
		
		log_indentation_down;
	end save_project;


	function exists (
	-- Returns true if the given module provides the given port.
	-- The module being searched in must be in the rig already.
		module			: in submodules.type_submodules.cursor;
		port			: in et_general.type_net_name.bounded_string; -- clock_output
		direction		: in submodules.type_netchanger_port_name) -- master/slave
		return boolean is

		result : boolean := false; -- to be returned
		
		use et_string_processing;
		use submodules;
		use et_schematic;
		
		submodule_file : type_submodule_path.bounded_string; -- $ET_TEMPLATES/motor_driver.mod
		module_name : type_module_name.bounded_string; 
		module_cursor : type_modules.cursor;

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
					when netlists.GLOBAL => 
						result := true;

					when netlists.LOCAL =>
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

		type_modules.query_element (
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
	-- Returns true if the given module provides the given device.
	-- The module being searched in must be in the rig already.						
		module	: in type_modules.cursor;
		device	: in type_name)
		return boolean is

		device_found : boolean := false; -- to be returned
		
		procedure query_devices (
			module_name	: in type_module_name.bounded_string;
			module		: in et_schematic.type_module) is
			use et_schematic.type_devices;
		begin
			if contains (module.devices, device) then
				device_found := true;
			end if;
		end query_devices;
		
	begin -- exists
		type_modules.query_element (
			position	=> module,
			process		=> query_devices'access);

		return device_found;
	end exists;
	
	function exists (
	-- Returns true if the given module provides the given submodule instance.
	-- The module being searched in must be in the rig already.						
		module		: in type_modules.cursor; -- the parent module that contains the submodule instance
		instance	: in et_general.type_module_instance_name.bounded_string) -- OSC1
		return boolean is

		instance_found : boolean := false; -- to be returned

		procedure query_submodules (
			module_name	: in type_module_name.bounded_string;
			module		: in et_schematic.type_module) is
			use submodules.type_submodules;
		begin
			if contains (module.submods, instance) then
				instance_found := true;
			end if;
		end query_submodules;

	begin -- exists
		-- search in the parent module for the given submodule instance
		type_modules.query_element (
			position	=> module,
			process		=> query_submodules'access);

		return instance_found;
	end exists;

	function exists (
	-- Returns true if the given submodule instance provides the
	-- given assembly variant. The submodule instance is searched for
	-- in the parent module indicated by cursor "module".
	-- The module being searched in must be in the rig already.												
		module		: in type_modules.cursor; -- the parent module that contains the submodule instance
		instance	: in et_general.type_module_instance_name.bounded_string; -- OSC1
		variant		: in et_general.type_variant_name.bounded_string) -- low_cost				
		return boolean is

		variant_found : boolean := false; -- to be returned

		procedure query_submodules (
			module_name	: in type_module_name.bounded_string;
			module		: in et_schematic.type_module) is
			use submodules;
			use submodules.type_submodules;
			submod_instance_cursor : submodules.type_submodules.cursor;
			submod_path : type_submodule_path.bounded_string;
			submod_name	: type_module_name.bounded_string;
			submod_cursor : type_modules.cursor;

			procedure query_variants (
			-- Locates the given assembly variant in the submodule.
			-- Sets flag variant_found.
				submodule_name	: in type_module_name.bounded_string;
				submodule		: in et_schematic.type_module) is
				use assembly_variants;
			begin
				if assembly_variants.pac_variants.contains (submodule.variants, variant) then
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
			type_modules.query_element (
				position	=> submod_cursor,
				process		=> query_variants'access);

		end query_submodules;
		
	begin -- exists
		-- search in the parent module for the given submodule instance
		type_modules.query_element (
			position	=> module,
			process		=> query_submodules'access);

		return variant_found;
	end exists;

	function exists (
	-- Returns true if the given module provides the given assembly variant.
	-- If the variant is an empty string then it is about the default variant
	-- which is always provided. The return is true in that case.
		module		: in type_modules.cursor;
		variant		: in et_general.type_variant_name.bounded_string) -- low_cost
		return boolean is

		use assembly_variants.pac_variants;

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
			
			type_modules.query_element (
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
		module	: in type_modules.cursor; -- the module like motor_driver
		variant	: in et_general.type_variant_name.bounded_string; -- low_cost				
		device	: in type_name)
		return boolean is

		result : boolean := false; -- to be returned

		procedure query_variants (
			module_name	: in type_module_name.bounded_string;
			module		: in et_schematic.type_module) is
			use assembly_variants.pac_variants;
			variant_cursor : assembly_variants.pac_variants.cursor;

			procedure query_devices (
				variant_name	: in et_general.type_variant_name.bounded_string;
				variant			: in assembly_variants.type_variant) is
				use assembly_variants;
				use assembly_variants.type_devices;
				device_cursor : assembly_variants.type_devices.cursor;
			begin
				device_cursor := find (variant.devices, device);

				-- The device may be listed in the assembly variant:
				if device_cursor /= assembly_variants.type_devices.no_element then
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

		type_modules.query_element (
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
		module	: in type_modules.cursor; -- the module like motor_driver
		variant	: in et_general.type_variant_name.bounded_string; -- low_cost				
		device	: in type_name)
		return assembly_variants.type_devices.cursor is

		cursor : assembly_variants.type_devices.cursor; -- to be returned;
		
		procedure query_variants (
			module_name	: in type_module_name.bounded_string;
			module		: in et_schematic.type_module) is
			use assembly_variants.pac_variants;
			
			variant_cursor : assembly_variants.pac_variants.cursor;

			procedure query_devices (
				variant_name	: in et_general.type_variant_name.bounded_string;
				variant			: in assembly_variants.type_variant) is
				use assembly_variants.type_devices;
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

		type_modules.query_element (
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
		module	: in type_modules.cursor; -- the module like motor_driver
		variant	: in et_general.type_variant_name.bounded_string; -- low_cost				
		submod	: in et_general.type_module_instance_name.bounded_string) -- OSC1
		return assembly_variants.type_submodules.cursor is

		cursor : assembly_variants.type_submodules.cursor; -- to be returned;
		
		procedure query_variants (
			module_name	: in type_module_name.bounded_string;
			module		: in et_schematic.type_module) is
			use assembly_variants.pac_variants;

			variant_cursor : assembly_variants.pac_variants.cursor;

			procedure query_submodules (
				variant_name	: in et_general.type_variant_name.bounded_string;
				variant			: in assembly_variants.type_variant) is
				use assembly_variants.type_submodules;
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
			cursor := assembly_variants.type_submodules.no_element;
		else
			type_modules.query_element (
				position	=> module,
				process		=> query_variants'access);
		end if;
		
		return cursor;
	end alternative_submodule;
	
	function deepest_conductor_layer (
		module	: in type_modules.cursor) -- the module like motor_driver
		return et_pcb_stack.type_signal_layer is
	begin
		return et_pcb_stack.deepest_layer (type_modules.element (module).board.stack);
	end deepest_conductor_layer;

end et_project;
	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
