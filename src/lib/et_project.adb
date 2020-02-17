------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                               PROJECT                                    --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2019 Mario Blunk, Blunk electronic                 --
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
with conventions;
with submodules;
with assembly_variants;
with material;
with netlists;
with et_text;					use et_text;
with et_geometry;				use et_geometry;
with general_rw;				use general_rw;
with pcb_rw;					use pcb_rw;
with schematic_rw;				use schematic_rw;
with symbol_rw;					use symbol_rw;
with device_rw;					use device_rw;
with et_symbols;
with et_devices;				use et_devices;
with et_frames;
with frame_rw;
with et_meta;

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
		log_threshold		: in et_string_processing.type_log_level) is 
		use et_string_processing;
		use type_modules;
		use general_rw;
		
		module_file_handle : ada.text_io.file_type;

		procedure write_header is 
		-- Creates the module/submodule file and writes a nice header in it.
			use ada.directories;
			use gnat.directory_operations;
			use type_project_name;
			use type_et_project_path;
			use et_general;
		
		begin -- write_module_header

			-- create module file and write in it a header
			create (
				file => module_file_handle,
				mode => out_file, 
				name => type_module_file_name.to_string (module_file_name));

			set_output (module_file_handle);
			put_line (comment_mark & " " & system_name & " module");
			put_line (comment_mark & " " & date);
			put_line (comment_mark & " " & row_separator_double);
			new_line;
		end write_header;		

		procedure write_footer is
		-- writes a nice footer in the module file and closes it.
		begin
			new_line;
			
			log (text => "closing module file ...", level => log_threshold + 1);
			
			put_line (comment_mark & " " & row_separator_double);
			put_line (comment_mark & " " & date);
			put_line (comment_mark & " module file end");
			new_line;

			set_output (standard_output);		
			close (module_file_handle);
		end write_footer;
		
		function rotation (pos : in et_pcb_coordinates.geometry.type_position'class) return string is -- CS make generic ?
			use et_pcb_coordinates.geometry;
		begin
			return to_string (rot (pos));
		end rotation;
		
		function face (point : et_pcb_coordinates.type_package_position) return string is
			use et_pcb_coordinates;
		begin
			return to_string (get_face (point));
		end face;

		procedure query_meta is
			use et_meta;
			meta : et_meta.type_meta := element (module_cursor).meta;

			procedure write_basic (basic : in type_basic'class) is begin
				write (keyword => keyword_company, parameters => to_string (basic.company), wrap => true);
				write (keyword => keyword_customer, parameters => to_string (basic.customer), wrap => true);
				write (keyword => keyword_partcode, parameters => to_string (basic.partcode));
				write (keyword => keyword_drawing_number, parameters => to_string (basic.drawing_number));
				write (keyword => keyword_revision, parameters => to_string (basic.revision));
				
				write (keyword => keyword_drawn_by, parameters => to_string (basic.drawn_by), wrap => true);
				write (keyword => keyword_drawn_date, parameters => to_string (basic.drawn_date));
				
				write (keyword => keyword_checked_by, parameters => to_string (basic.checked_by), wrap => true);
				write (keyword => keyword_checked_date, parameters => to_string (basic.checked_date));

				write (keyword => keyword_approved_by, parameters => to_string (basic.approved_by), wrap => true);
				write (keyword => keyword_approved_date, parameters => to_string (basic.approved_date));
			end;
		
		begin -- query_meta
			log_indentation_up;
			log (text => "meta data ...", level => log_threshold + 1);
			
			section_mark (section_meta, HEADER);

			-- schematic related
			section_mark (section_schematic, HEADER);
			write_basic (meta.schematic);
			-- CS write schematic specific meta stuff here
			section_mark (section_schematic, FOOTER);
			
			-- board related
			section_mark (section_board, HEADER);
			write_basic (meta.board);
			-- CS write schematic specific meta stuff here
			section_mark (section_board, FOOTER);
			
			section_mark (section_meta, FOOTER);

			log_indentation_down;
		end query_meta;
		
		procedure query_net_classes is
			use et_pcb;
			use et_pcb.type_net_classes;
			use et_pcb_coordinates.geometry;

			procedure write (class_cursor : in type_net_classes.cursor) is begin
				log (text => "net class " & to_string (key (class_cursor)), level => log_threshold + 1);
				section_mark (section_net_class, HEADER);

				write (keyword => keyword_name, parameters => to_string (key (class_cursor)));
				write (keyword => keyword_description, parameters => et_pcb.to_string (element (class_cursor).description), wrap => true);
				write (keyword => keyword_clearance, parameters => to_string (element (class_cursor).clearance));
				write (keyword => keyword_track_width_min, parameters => to_string (element (class_cursor).track_width_min));
				write (keyword => keyword_via_drill_min, parameters => to_string (element (class_cursor).via_drill_min));
				write (keyword => keyword_via_restring_min, parameters => to_string (element (class_cursor).via_restring_min));
				write (keyword => keyword_micro_via_drill_min, parameters => to_string (element (class_cursor).micro_via_drill_min));
				write (keyword => keyword_micro_via_restring_min, parameters => to_string (element (class_cursor).micro_via_restring_min));

				section_mark (section_net_class, FOOTER);
			end write;
		
		begin -- query_net_classes
			log_indentation_up;
			
			section_mark (section_net_classes, HEADER);
			iterate (element (module_cursor).net_classes, write'access);
			section_mark (section_net_classes, FOOTER);

			log_indentation_down;
		end query_net_classes;

		procedure query_drawing_grid is 
			use et_coordinates.geometry;
			use et_pcb_coordinates.geometry;
		begin
			log_indentation_up;
			
			section_mark (section_drawing_grid, HEADER);

			section_mark (section_schematic, HEADER);
			write (keyword => keyword_default, parameters => 
				   keyword_x & to_string (element (module_cursor).grid.x) & space &
				   keyword_y & to_string (element (module_cursor).grid.y));
			section_mark (section_schematic, FOOTER);

			section_mark (section_board, HEADER);
			write (keyword => keyword_default, parameters => 
				   keyword_x & to_string (element (module_cursor).board.grid.x) & space &
				   keyword_y & to_string (element (module_cursor).board.grid.y));
			section_mark (section_board, FOOTER);
			
			section_mark (section_drawing_grid, FOOTER);

			log_indentation_down;
		end query_drawing_grid;

		procedure query_layer_stack is
			use et_pcb_coordinates.geometry;
			use et_pcb_stack;
			use package_layers;

			procedure query_layers (cursor : in package_layers.cursor) is
				layer : type_layer := element (cursor);
			begin
				write (keyword => keyword_conductor,
					   parameters => 2 * space & to_string (to_index (cursor)) & to_string (layer.conductor.thickness));
				
				write (keyword => keyword_dielectric, 
					   parameters => space & to_string (to_index (cursor)) & to_string (layer.dielectric.thickness));

			end;

			bottom_layer : type_signal_layer;
			bottom_layer_thickness : type_conductor_thickness;
			
		begin -- query_layer_stack
			log_indentation_up;
			
			section_mark (section_board_layer_stack, HEADER);

			-- iterate layers starting at top layer (1) until the deepest inner layer.
			-- The bottom layer is not part of the layer list and will be written later.
			iterate (element (module_cursor).board.stack.layers, query_layers'access);

			-- The bottom layer number is the deepest inner layer plus one:
			bottom_layer := last_index (element (module_cursor).board.stack.layers) + 1;

			-- Get the bottom conductor thickness:
			bottom_layer_thickness := element (module_cursor).board.stack.bottom.thickness;

			-- Write the bottom layer in the file.
			write (keyword => keyword_conductor,
				parameters => space & to_string (bottom_layer) & to_string (bottom_layer_thickness) &
				space & comment_mark & " bottom signal layer");
			
			section_mark (section_board_layer_stack, FOOTER);
			
			log_indentation_down;
			
		end query_layer_stack;
			
		procedure query_nets is
			use type et_schematic.type_net;
			use et_schematic.type_nets;
			use et_pcb;

			procedure query_strands (
				net_name	: in type_net_name.bounded_string;
				net			: in et_schematic.type_net) is
				use et_schematic;
				use type_strands;
				use et_coordinates.geometry;
				strand_cursor : type_strands.cursor := net.strands.first;

				procedure query_segments (strand : in type_strand) is
					use type_net_segments;
					segment_cursor : type_net_segments.cursor := strand.segments.first;

					use type_ports_device;
					use type_ports_submodule;

					use netlists;
					use type_ports_netchanger;
					
					procedure query_labels (segment : in type_net_segment) is
						use type_net_labels;
						label_cursor : type_net_labels.cursor := segment.labels.first;
						use et_symbols;
					begin -- query_labels
						if not is_empty (segment.labels) then
							section_mark (section_labels, HEADER);
							while label_cursor /= type_net_labels.no_element loop
								section_mark (section_label, HEADER);
								
								write (keyword => keyword_position, parameters => position (element (label_cursor).position));
								write (keyword => keyword_rotation, parameters => to_string (element (label_cursor).rotation));
								write (keyword => et_text.keyword_size, parameters => to_string (element (label_cursor).size));
								write (keyword => keyword_style, parameters => to_string (element (label_cursor).style));
								write (keyword => et_text.keyword_line_width, parameters => to_string (element (label_cursor).width));

								write (keyword => keyword_appearance, parameters =>
									et_schematic.to_string (appearance => element (label_cursor).appearance));
								
								-- a tag label also indicates a signal direction
								if element (label_cursor).appearance = TAG then
									write (keyword => et_schematic.keyword_direction, parameters => to_string (element (label_cursor).direction));
								end if;
								
								section_mark (section_label, FOOTER);
								next (label_cursor);
							end loop;
							section_mark (section_labels, FOOTER);
						end if;
					end query_labels;

					procedure query_junctions (segment : in type_net_segment) is begin
						if segment.junctions.start_point then
							write (keyword => keyword_junction, parameters => keyword_start);
						end if;

						if segment.junctions.end_point then
							write (keyword => keyword_junction, parameters => keyword_end);
						end if;
					end query_junctions;
					
					procedure query_device_ports (segment : in type_net_segment) is
						use et_symbols;
						port_cursor : type_ports_device.cursor := segment.ports_devices.first;
					begin -- query_device_ports
						while port_cursor /= type_ports_device.no_element loop
							write (keyword => keyword_device, parameters => 
								space & to_string (element (port_cursor).device_name)
								& space & keyword_port & space
								& to_string (element (port_cursor).port_name)
								); -- device IC1 port A
							next (port_cursor);
						end loop;
					end query_device_ports;

					procedure query_submodule_ports (segment : in type_net_segment) is
						use et_symbols;
						port_cursor : type_ports_submodule.cursor := segment.ports_submodules.first;
					begin
						while port_cursor /= type_ports_submodule.no_element loop

							write (keyword => keyword_submodule, parameters => 
								space & to_string (element (port_cursor).module_name)
								& space & keyword_port & space
								& et_general.to_string (element (port_cursor).port_name)
								); -- submodule CLK_GENERATOR port out

							next (port_cursor);
						end loop;
					end query_submodule_ports;
					
					procedure query_netchanger_ports (segment : in type_net_segment) is
						use et_symbols;
						port_cursor : type_ports_netchanger.cursor := segment.ports_netchangers.first;
					begin
						while port_cursor /= type_ports_netchanger.no_element loop

							write (keyword => keyword_netchanger, parameters => 
								submodules.to_string (element (port_cursor).index)
								& space & keyword_port
								& submodules.to_string (element (port_cursor).port)
								); -- netchanger 1 port master/slave

							next (port_cursor);
						end loop;
					end query_netchanger_ports;

				begin -- query_segments
					section_mark (section_segments, HEADER);
					while segment_cursor /= type_net_segments.no_element loop
						section_mark (section_segment, HEADER);

						write (keyword => keyword_start, parameters => position (element (segment_cursor).start_point));
						write (keyword => keyword_end,   parameters => "  " & position (element (segment_cursor).end_point));

						query_element (segment_cursor, query_labels'access);
						query_element (segment_cursor, query_junctions'access);

						-- write ports there are any. otherwise leave out section ports.
						if 	is_empty (element (segment_cursor).ports_devices) and
							is_empty (element (segment_cursor).ports_submodules) and
							is_empty (element (segment_cursor).ports_netchangers) then
							null;
						else
							section_mark (section_ports, HEADER);
							query_element (segment_cursor, query_device_ports'access);
							query_element (segment_cursor, query_submodule_ports'access);
							query_element (segment_cursor, query_netchanger_ports'access);
							section_mark (section_ports, FOOTER);
						end if;
							
						section_mark (section_segment, FOOTER);
						next (segment_cursor);
					end loop;
					section_mark (section_segments, FOOTER);
				end query_segments;
				
			begin -- query_strands
				section_mark (section_strands, HEADER);
				while strand_cursor /= type_strands.no_element loop
					section_mark (section_strand, HEADER);

					write (keyword => keyword_position, parameters => schematic_rw.position (element (strand_cursor).position));

					query_element (strand_cursor, query_segments'access);
					
					section_mark (section_strand, FOOTER);
					next (strand_cursor);
				end loop;
				
				section_mark (section_strands, FOOTER);
			end query_strands;

			procedure query_route (
			-- This is about routed tracks/traces and zones in the board:
				net_name	: in type_net_name.bounded_string;
				net			: in et_schematic.type_net) is
				use et_packages;
				use et_packages.pac_shapes;
				use et_pcb;
				use et_pcb_stack;
				use et_pcb_coordinates.geometry;
				
				use pac_copper_lines;
				line_cursor : pac_copper_lines.cursor := net.route.lines.first;

				use pac_copper_arcs;
				arc_cursor : pac_copper_arcs.cursor := net.route.arcs.first;

				use pac_vias;
				via_cursor : pac_vias.cursor := net.route.vias.first;

				use pac_signal_polygons_solid; 
				use pac_signal_polygons_hatched;
				use et_pcb.pac_copper_cutouts;
				polygon_solid_cursor : pac_signal_polygons_solid.cursor := net.route.polygons_2.solid.first;
				polygon_hatched_cursor : pac_signal_polygons_hatched.cursor := net.route.polygons_2.hatched.first;
				cutout_zone_cursor : et_pcb.pac_copper_cutouts.cursor := net.route.cutouts.first;
				
			begin -- query_route
				section_mark (section_route, HEADER);

				while line_cursor /= pac_copper_lines.no_element loop
					section_mark (section_line, HEADER);
					
					write (keyword => keyword_start, parameters => position (element (line_cursor).start_point));
					write (keyword => keyword_end  , parameters => position (element (line_cursor).end_point));
					write (keyword => keyword_layer, parameters => to_string (element (line_cursor).layer));
					write (keyword => pcb_rw.keyword_width, parameters => to_string (element (line_cursor).width));

					section_mark (section_line, FOOTER);
					next (line_cursor);
				end loop;

				while arc_cursor /= pac_copper_arcs.no_element loop
					section_mark (section_arc, HEADER);

					write (keyword => keyword_center, parameters => position (element (arc_cursor).center));
					write (keyword => keyword_start , parameters => position (element (arc_cursor).start_point));
					write (keyword => keyword_end   , parameters => position (element (arc_cursor).end_point));
					write (keyword => pcb_rw.keyword_width , parameters => to_string (element (arc_cursor).width));
					write (keyword => keyword_layer , parameters => to_string (element (arc_cursor).layer));
					
					section_mark (section_arc, FOOTER);
					next (arc_cursor);
				end loop;

				while via_cursor /= pac_vias.no_element loop
					section_mark (section_via, HEADER);

					write (keyword => keyword_position, parameters => position (element (via_cursor).position));
					write (keyword => keyword_diameter, parameters => to_string (element (via_cursor).diameter));
					write (keyword => keyword_layer_start, parameters => to_string (element (via_cursor).layer_start));
					write (keyword => keyword_layer_end  , parameters => to_string (element (via_cursor).layer_end));
					write (keyword => keyword_restring_outer_layers, parameters => to_string (element (via_cursor).restring_outer));
					write (keyword => keyword_restring_inner_layers, parameters => to_string (element (via_cursor).restring_inner));
					
					section_mark (section_via, FOOTER);
					next (via_cursor);
				end loop;

				-- solid fill zones
				while polygon_solid_cursor /= pac_signal_polygons_solid.no_element loop
					fill_zone_begin;

					write_easing (element (polygon_solid_cursor).easing);
					
					write_width_min (element (polygon_solid_cursor).width_min);
					write_isolation (element (polygon_solid_cursor).isolation);
					
					write_priority (element (polygon_solid_cursor).priority_level);
					write_signal_layer (element (polygon_solid_cursor).layer);

					write_fill_stlye (SOLID);

					case element (polygon_solid_cursor).connection is
						when et_pcb.THERMAL => 
							write_pad_connection (element (polygon_solid_cursor).connection);
							write_thermal (element (polygon_solid_cursor).thermal);
			
						when et_pcb.SOLID =>
							write_pad_technology (element (polygon_solid_cursor).technology);
							
					end case;

					contours_begin;
					write_polygon_segments (pac_shapes.type_polygon_base (element (polygon_solid_cursor)));
					contours_end;
					
					fill_zone_end;
					next (polygon_solid_cursor);
				end loop;

				-- hatched fill zones
				while polygon_hatched_cursor /= pac_signal_polygons_hatched.no_element loop
					fill_zone_begin;

					write_easing (element (polygon_hatched_cursor).easing);

					write_width_min (element (polygon_hatched_cursor).width_min);
					write_isolation (element (polygon_hatched_cursor).isolation);
					
					write_priority (element (polygon_hatched_cursor).priority_level);
					write_signal_layer (element (polygon_hatched_cursor).layer);

					write_fill_stlye (HATCHED);
					
					write_hatching (element (polygon_hatched_cursor).hatching);

					case element (polygon_hatched_cursor).connection is
						when et_pcb.THERMAL => 
							write_pad_connection (element (polygon_hatched_cursor).connection);
							write_thermal (element (polygon_hatched_cursor).thermal);
			
						when et_pcb.SOLID =>
							write_pad_technology (element (polygon_hatched_cursor).technology);

					end case;

					contours_begin;
					write_polygon_segments (pac_shapes.type_polygon_base (element (polygon_hatched_cursor)));
					contours_end;
					
					fill_zone_end;
					next (polygon_hatched_cursor);
				end loop;

				-- cutout zones
				while cutout_zone_cursor /= et_pcb.pac_copper_cutouts.no_element loop
					cutout_zone_begin;
					write_signal_layer (element (cutout_zone_cursor).layer);
					write_easing (element (cutout_zone_cursor).easing);

					contours_begin;
					write_polygon_segments (pac_shapes.type_polygon_base (element (cutout_zone_cursor)));
					contours_end;
					
					cutout_zone_end;
					next (cutout_zone_cursor);
				end loop;
				
				section_mark (section_route, FOOTER);
			end query_route;

			procedure write (net_cursor : in et_schematic.type_nets.cursor) is begin
				log (text => "net " & et_general.to_string (key (net_cursor)), level => log_threshold + 1);
				section_mark (section_net, HEADER);

				write (keyword => keyword_name, parameters => et_general.to_string (key (net_cursor)));
				write (keyword => keyword_class, parameters => to_string (element (net_cursor).class));
				write (keyword => keyword_scope, parameters => netlists.to_string (element (net_cursor).scope));

				query_element (net_cursor, query_strands'access);
				query_element (net_cursor, query_route'access);
				
				section_mark (section_net, FOOTER);
				new_line;
			end write;
			
		begin -- query_nets
			log_indentation_up;

			section_mark (section_nets, HEADER);
			iterate (element (module_cursor).nets, write'access);
			section_mark (section_nets, FOOTER);
			
			log_indentation_down;
		end query_nets;

		procedure query_devices is		
			use et_schematic;
			use et_symbols;
			use et_schematic.type_devices;

			procedure query_units (device_name : in type_name; device : in et_schematic.type_device) is
				use et_schematic.type_units;
				unit_cursor : type_units.cursor := device.units.first;

				use et_coordinates.geometry;
				
				procedure write_placeholder (ph : in type_text_placeholder) is begin
					section_mark (section_placeholder, HEADER);
					write (keyword => keyword_meaning, parameters => to_string (ph.meaning));
					write (keyword => keyword_position, parameters => position (ph.position));
					write_text_properties (ph);
					section_mark (section_placeholder, FOOTER);
				end write_placeholder;

				use et_devices;
				
			begin -- query_units
				section_mark (section_units, HEADER);
				while unit_cursor /= type_units.no_element loop
					section_mark (section_unit, HEADER);
					write (keyword => keyword_name, parameters => to_string (key (unit_cursor)));
					write (keyword => keyword_position, parameters => position (element (unit_cursor).position)); -- position sheet 1 x 147.32 y 96.97
					--write (keyword => keyword_rotation, parameters => to_string (element (unit_cursor).rotation)); -- rotation 180.0
					write (keyword => keyword_rotation, parameters => to_string (rot (element (unit_cursor).position))); -- rotation 180.0
					write (keyword => keyword_mirrored, parameters => to_string (element (unit_cursor).mirror, verbose => false)); -- x_axis, y_axis, none

					if element (unit_cursor).appearance = PCB then
						section_mark (section_placeholders, HEADER);
						
						write_placeholder (element (unit_cursor).name);
						write_placeholder (element (unit_cursor).value);
						write_placeholder (element (unit_cursor).purpose);
						--write_placeholder (element (unit_cursor).partcode);

						section_mark (section_placeholders, FOOTER);
					end if;
					
					section_mark (section_unit, FOOTER);
					next (unit_cursor);
				end loop;
				section_mark (section_units, FOOTER);
			end query_units;

			procedure query_placeholders (
				device_name : in type_name;
				device 		: in et_schematic.type_device) is
				use et_pcb_coordinates;
				use et_packages;
				use pac_text_placeholders;
				placeholder_cursor : pac_text_placeholders.cursor;

				face : et_pcb_coordinates.type_face;
				layer : type_placeholder_package_layer;
				
				procedure write_placeholder (placeholder_cursor : in pac_text_placeholders.cursor) is 
				begin
					section_mark (section_placeholder, HEADER);
					write (keyword => et_pcb_stack.keyword_layer, parameters => to_string (layer));
					write (keyword => keyword_meaning, parameters => to_string (element (placeholder_cursor).meaning));
					write_text_properties_with_face (element (placeholder_cursor), face);
					section_mark (section_placeholder, FOOTER);
				end write_placeholder;
				
			begin -- query_placeholders
				section_mark (section_placeholders, HEADER);

				layer := SILK_SCREEN;
				face := TOP;
				device.text_placeholders.silk_screen.top.iterate (write_placeholder'access);

				face := BOTTOM;				
				device.text_placeholders.silk_screen.bottom.iterate (write_placeholder'access);

				layer := ASSEMBLY_DOCUMENTATION;
				face := TOP;				
				device.text_placeholders.assy_doc.top.iterate (write_placeholder'access);

				face := BOTTOM;
				device.text_placeholders.assy_doc.bottom.iterate (write_placeholder'access);
				
				section_mark (section_placeholders, FOOTER);				
			end query_placeholders;

			procedure write (device_cursor : in et_schematic.type_devices.cursor) is begin
				section_mark (section_device, HEADER);
				write (keyword => keyword_name, parameters => to_string (key (device_cursor)));
				write (keyword => keyword_appearance, parameters => to_string (element (device_cursor).appearance));
				write (keyword => keyword_model, parameters => to_string (element (device_cursor).model));

				case element (device_cursor).appearance is
					when PCB =>
						write (keyword => keyword_value   , parameters => to_string (element (device_cursor).value));
						write (keyword => keyword_variant , parameters => to_string (element (device_cursor).variant));
						write (keyword => material.keyword_partcode, parameters => material.to_string (element (device_cursor).partcode));
						write (keyword => keyword_purpose , parameters => to_string (element (device_cursor).purpose), wrap => true);
						
						section_mark (section_package, HEADER);

						-- This is the position of the package in the layout, 
						write (keyword => keyword_position, parameters => -- position x 34.5 y 60.1 face top/bottom
							   position (element (device_cursor).position));

						-- Flip status:
						write (keyword => keyword_flipped, parameters => et_pcb.to_string (element (device_cursor).flipped));
					
						query_element (device_cursor, query_placeholders'access);
						section_mark (section_package, FOOTER);
						
					when VIRTUAL => null;
				end case;

				query_element (device_cursor, query_units'access);
				
				section_mark (section_device, FOOTER);
				new_line;
			end write;
			
		begin -- query_devices
			section_mark (section_devices, HEADER);
			iterate (element (module_cursor).devices, write'access);
			section_mark (section_devices, FOOTER);
		end query_devices;

		procedure query_assembly_variants is
		-- writes the assembly variants in the module file
			use assembly_variants;
			use assembly_variants.pac_variants;

			procedure query_devices (
				variant_name	: in et_general.type_variant_name.bounded_string;
				variant			: in assembly_variants.type_variant) is
				use assembly_variants.type_devices;
				device_cursor : assembly_variants.type_devices.cursor := variant.devices.first;

				function purpose return string is 
					use et_devices;
					use type_purpose;
				begin
					if length (element (device_cursor).purpose) > 0 then
						return space & keyword_purpose & space &
							enclose_in_quotes (
								text_in => et_devices.to_string (element (device_cursor).purpose),
								quote	=> latin_1.quotation);
					else
						return "";
					end if;
				end;

				use et_devices;
				
			begin -- query_devices
				while device_cursor /= assembly_variants.type_devices.no_element loop
					case element (device_cursor).mounted is
						when NO =>
							write (
								keyword		=> keyword_device,
								parameters	=> to_string (key (device_cursor)) & 
												space & keyword_not_mounted);

						when YES =>
							write (
								keyword		=> keyword_device,
								parameters	=> to_string (key (device_cursor)) & 
									space &
									keyword_value & space &
									to_string (element (device_cursor).value) &
									space & material.keyword_partcode & space &
									material.to_string (element (device_cursor).partcode) &
									purpose);

					end case;
					
					next (device_cursor);
				end loop;
			end query_devices;

			procedure query_submodules (
				variant_name	: in et_general.type_variant_name.bounded_string;
				variant			: in assembly_variants.type_variant) is
				use assembly_variants;
				use assembly_variants.type_submodules;
				submodule_cursor : assembly_variants.type_submodules.cursor := variant.submodules.first;
			begin
				while submodule_cursor /= type_submodules.no_element loop
					write (
						keyword		=> keyword_submodule,
						parameters	=> et_general.to_string (key (submodule_cursor)) &
										space & keyword_variant & space &
										to_variant (element (submodule_cursor).variant));
					
					next (submodule_cursor);
				end loop;
			end query_submodules;

			procedure write (variant_cursor : in assembly_variants.pac_variants.cursor) is begin
				section_mark (section_assembly_variant, HEADER);
				write (keyword => keyword_name, parameters => to_variant (key (variant_cursor)));
				write (keyword => keyword_description, wrap => true, parameters => to_string (element (variant_cursor).description));

				-- write the device variants
				query_element (
					position	=> variant_cursor,
					process		=> query_devices'access);

				-- write the submodule variants
				query_element (
					position	=> variant_cursor,
					process		=> query_submodules'access);
				
				section_mark (section_assembly_variant, FOOTER);
				new_line;
			end write;
			
		begin -- query_assembly_variants
			section_mark (section_assembly_variants, HEADER);			
			iterate (element (module_cursor).variants, write'access);

			-- write the active assembly variant
			write (
				keyword		=> keyword_active,
				parameters	=> et_general.to_variant (element (module_cursor).active_variant));

			section_mark (section_assembly_variants, FOOTER);
		end query_assembly_variants;
		
		procedure query_netchangers is
		-- writes the netchangers in the module file
			use submodules;
			use type_netchangers;

			procedure query_netchanger (cursor : type_netchangers.cursor) is
			begin
				section_mark (section_netchanger, HEADER);
				write (keyword => keyword_name,	parameters => to_string (key (cursor))); -- 1, 2, 201, ...
				write (keyword => keyword_position_in_schematic, parameters => position (element (cursor).position_sch)); -- position_in_schematic sheet 1 x 147.32 y 96.97
				write (keyword => keyword_rotation_in_schematic, parameters => geometry.to_string (geometry.rot (element (cursor).position_sch))); -- rotation_in_schematic 90.0
				write (keyword => keyword_position_in_board, parameters => position (element (cursor).position_brd)); -- position_in_board x 1.32 y 6.97
				write (keyword => et_pcb_stack.keyword_layer, parameters => et_pcb_stack.to_string (element (cursor).layer)); -- layer 2
				section_mark (section_netchanger, FOOTER);
			end query_netchanger;
			
		begin
			section_mark (section_netchangers, HEADER);
			iterate (element (module_cursor).netchangers, query_netchanger'access);
			section_mark (section_netchangers, FOOTER);
		end query_netchangers;
		
		procedure query_frames is begin
			-- schematic frames:
			section_mark (section_drawing_frames, HEADER);
			section_mark (section_schematic, HEADER);
			write (
				keyword 	=> keyword_template, 
				parameters	=> et_frames.to_string (element (module_cursor).frames.template));
			
			-- CS frame count ?
			-- CS description ?
			
			section_mark (section_schematic, FOOTER);			

			-- board frame:
			section_mark (section_board, HEADER);
			
			write (
				keyword		=> keyword_template, 
				parameters	=> et_frames.to_string (element (module_cursor).board.frame.template));
			
			section_mark (section_board, FOOTER);			
			section_mark (section_drawing_frames, FOOTER);
		end query_frames;

		procedure query_submodules is		
			use et_schematic;
			use submodules;
			use type_submodules;

			procedure query_ports (port_cursor : in submodules.type_submodule_ports.cursor) is
				use type_submodule_ports;
			begin
				section_mark (section_port, HEADER);
				write (keyword => keyword_name, parameters => et_general.to_string (key (port_cursor))); -- name clk_out
				write (keyword => keyword_position, parameters => position (element (port_cursor).position)); -- position x 0 y 10
				write (keyword => submodules.keyword_direction, parameters => to_string (element (port_cursor).direction)); -- direction master/slave
				section_mark (section_port, FOOTER);
			end;

			procedure write (submodule_cursor : in type_submodules.cursor) is 
				use et_coordinates.geometry;
			begin
				section_mark (section_submodule, HEADER);
				write (keyword => keyword_name, parameters => et_general.to_string (key (submodule_cursor))); -- name stepper_driver_1
				write (keyword => keyword_file, parameters => type_submodule_path.to_string (element (submodule_cursor).file)); -- file $ET_TEMPLATES/motor_driver.mod

				write (keyword => keyword_position, parameters => position (element (submodule_cursor).position));
				write (keyword => submodules.keyword_size, parameters => 
					space & keyword_x & to_string (element (submodule_cursor).size.x) &
					space & keyword_y & to_string (element (submodule_cursor).size.y)); -- size x 50 y 70
				
				write (keyword => keyword_position_in_board, parameters => -- position_in_board x 23 y 0.2 rotation 90.0
					position (element (submodule_cursor).position_in_board));

				write (keyword => keyword_view_mode, parameters => to_string (element (submodule_cursor).view_mode));

				section_mark (section_ports, HEADER);
				type_submodule_ports.iterate (element (submodule_cursor).ports, query_ports'access);
				section_mark (section_ports, FOOTER);
				
				section_mark (section_submodule, FOOTER);				
			end write;
			
		begin -- query_submodules
			section_mark (section_submodules, HEADER);
			iterate (element (module_cursor).submods, write'access);
			section_mark (section_submodules, FOOTER);
		end query_submodules;

		procedure query_texts is		
			use et_schematic;
			use type_texts;

			procedure write (text_cursor : in type_texts.cursor) is begin
				section_mark (section_text, HEADER);
				write (keyword => keyword_position, parameters => position (element (text_cursor).position));
				write (keyword => keyword_content, wrap => true,
					   parameters => to_string (element (text_cursor).content));
				write_text_properties (element (text_cursor));
				section_mark (section_text, FOOTER);
			end write;
		
		begin
			section_mark (section_texts, HEADER);
			iterate (element (module_cursor).texts, write'access);
			section_mark (section_texts, FOOTER);
		end query_texts;

		procedure query_board is
			use et_packages;
			use et_pcb;
			use et_pcb_stack;
			use et_pcb_coordinates.geometry;

			use type_texts_with_content;
			use et_pcb.pac_text_placeholders;

			use type_silk_lines;
			use type_silk_arcs;
			use type_silk_circles;
			use pac_silk_polygons;

			use type_doc_lines;
			use type_doc_arcs;
			use type_doc_circles;
			use pac_doc_polygons;

			use type_stencil_lines;
			use type_stencil_arcs;
			use type_stencil_circles;
			use type_stencil_polygons;

			use type_stop_lines;
			use type_stop_arcs;
			use type_stop_circles;
			use type_stop_polygons;

			use type_keepout_lines;
			use type_keepout_arcs;
			use type_keepout_circles;
			use type_keepout_polygons;

			use type_route_restrict_lines;
			use type_route_restrict_arcs;
			use type_route_restrict_circles;
			use type_route_restrict_polygons;

			use type_via_restrict_lines;
			use type_via_restrict_arcs;
			use type_via_restrict_circles;
			use type_via_restrict_polygons;

			use pac_texts;

			use et_pcb.type_pcb_contour_lines;
			use et_pcb.type_pcb_contour_arcs;
			use et_pcb.type_pcb_contour_circles;
			
			-- general stuff
			use type_text_placeholders_copper;
			procedure write_placeholder (cursor : in et_pcb.pac_text_placeholders.cursor) is
			begin
				placeholder_begin;
				write (keyword => keyword_meaning, parameters => to_string (element (cursor).meaning));
				write_text_properties (element (cursor));
				placeholder_end;
			end write_placeholder;

			-- COPPER (NON-ELECTRIC) in any signal layers
			use pac_copper_lines;
			procedure write_line (cursor : in pac_copper_lines.cursor) is begin
				line_begin;
				write_line (element (cursor));
				write_width (element (cursor).width);
				write_signal_layer (element (cursor).layer);
				line_end;
			end;

			use pac_copper_arcs;
			procedure write_arc (cursor : in pac_copper_arcs.cursor) is begin
				arc_begin;
				write_arc (element (cursor));
				write_width (element (cursor).width);
				write_signal_layer (element (cursor).layer);
				arc_end;
			end;

			use et_pcb.pac_copper_circles;
			procedure write_circle (cursor : in et_pcb.pac_copper_circles.cursor) is begin
				write_circle_copper (element (cursor));
			end;

			-- solid fill zones in copper
			use pac_copper_polygons_floating_solid;
			procedure write_polygon (cursor : in pac_copper_polygons_floating_solid.cursor) is begin
				fill_zone_begin;

				write_easing (element (cursor).easing);

				write_width_min (element (cursor).width_min);
				write_isolation (element (cursor).isolation);

				write_priority (element (cursor).priority_level);
				write_signal_layer (element (cursor).layer);

				write_fill_stlye (element (cursor).fill_style);

				write_polygon_segments (pac_shapes.type_polygon_base (element (cursor)));

				fill_zone_end;
			end;

			-- hatched fill zones in copper
			use pac_copper_polygons_floating_hatched;
			procedure write_polygon (cursor : in pac_copper_polygons_floating_hatched.cursor) is begin
				fill_zone_begin;

				write_easing (element (cursor).easing);

				write_width_min (element (cursor).width_min);
				write_isolation (element (cursor).isolation);

				write_priority (element (cursor).priority_level);
				write_signal_layer (element (cursor).layer);

				write_fill_stlye (element (cursor).fill_style);
				write_hatching (element (cursor).hatching);

				write_polygon_segments (pac_shapes.type_polygon_base (element (cursor)));

				fill_zone_end;
			end;

			-- cutout zones in any signal layers
			use et_pcb.pac_copper_cutouts;
			procedure write_cutout (cursor : in et_pcb.pac_copper_cutouts.cursor) is begin
				cutout_zone_begin;
				write_signal_layer (element (cursor).layer);
				write_easing (element (cursor).easing);
				write_polygon_segments (pac_shapes.type_polygon_base (element (cursor)));
				cutout_zone_end;
			end;

			-- texts any signal layers
			procedure write_text (cursor : in pac_texts.cursor) is begin
				text_begin;
				write (keyword => keyword_content, parameters => to_string (element (cursor).content));
				write_text_properties (element (cursor));
				write (keyword => keyword_layer, parameters => to_string (element (cursor).layer));
				text_end;
			end write_text;

			-- text placeholders in any signal layers
			procedure write_placeholder (cursor : in type_text_placeholders_copper.cursor) is begin
				placeholder_begin;
				write (keyword => keyword_meaning, parameters => to_string (element (cursor).meaning));
				write_text_properties (element (cursor));
				write (keyword => keyword_layer, parameters => to_string (element (cursor).layer));
				placeholder_end;
			end write_placeholder;

			use et_packages.pac_silk_cutouts;
			use et_packages.pac_doc_cutouts;
			use et_packages.pac_stencil_cutouts;
			use et_packages.pac_stop_cutouts;
			use et_packages.pac_keepout_cutouts;
			use et_packages.pac_route_restrict_cutouts;
			use et_packages.pac_via_restrict_cutouts;
			
		begin -- query_board
			section_mark (section_board, HEADER);

			-- SILK SCREEN
			section_mark (section_silk_screen, HEADER);

				section_mark (section_top, HEADER);
				iterate (element (module_cursor).board.silk_screen.top.lines, write_line'access);
				iterate (element (module_cursor).board.silk_screen.top.arcs, write_arc'access);
				iterate (element (module_cursor).board.silk_screen.top.circles, write_circle'access);
				iterate (element (module_cursor).board.silk_screen.top.polygons, write_polygon'access);
				iterate (element (module_cursor).board.silk_screen.top.cutouts, write_cutout'access);
				iterate (element (module_cursor).board.silk_screen.top.texts, write_text'access);
				iterate (element (module_cursor).board.silk_screen.top.placeholders, write_placeholder'access);
				section_mark (section_top, FOOTER);

				section_mark (section_bottom, HEADER);
				iterate (element (module_cursor).board.silk_screen.bottom.lines, write_line'access);
				iterate (element (module_cursor).board.silk_screen.bottom.arcs, write_arc'access);
				iterate (element (module_cursor).board.silk_screen.bottom.circles, write_circle'access);
				iterate (element (module_cursor).board.silk_screen.bottom.polygons, write_polygon'access);
				iterate (element (module_cursor).board.silk_screen.bottom.cutouts, write_cutout'access);
				iterate (element (module_cursor).board.silk_screen.bottom.texts, write_text'access);
				iterate (element (module_cursor).board.silk_screen.bottom.placeholders, write_placeholder'access);
				section_mark (section_bottom, FOOTER);
			
			section_mark (section_silk_screen, FOOTER);

			-- ASSEMBLY DOCUMENTATION
			section_mark (section_assembly_doc, HEADER);

			section_mark (section_top, HEADER);
				iterate (element (module_cursor).board.assy_doc.top.lines, write_line'access);
				iterate (element (module_cursor).board.assy_doc.top.arcs, write_arc'access);
				iterate (element (module_cursor).board.assy_doc.top.circles, write_circle'access);
				iterate (element (module_cursor).board.assy_doc.top.polygons, write_polygon'access);
				iterate (element (module_cursor).board.assy_doc.top.cutouts, write_cutout'access);
				iterate (element (module_cursor).board.assy_doc.top.texts, write_text'access);
				iterate (element (module_cursor).board.assy_doc.top.placeholders, write_placeholder'access);
				section_mark (section_top, FOOTER);

				section_mark (section_bottom, HEADER);
				iterate (element (module_cursor).board.assy_doc.bottom.lines, write_line'access);
				iterate (element (module_cursor).board.assy_doc.bottom.arcs, write_arc'access);
				iterate (element (module_cursor).board.assy_doc.bottom.circles, write_circle'access);
				iterate (element (module_cursor).board.assy_doc.bottom.polygons, write_polygon'access);
				iterate (element (module_cursor).board.assy_doc.bottom.cutouts, write_cutout'access);
				iterate (element (module_cursor).board.assy_doc.bottom.texts, write_text'access);
				iterate (element (module_cursor).board.assy_doc.bottom.placeholders, write_placeholder'access);
				section_mark (section_bottom, FOOTER);

			section_mark (section_assembly_doc, FOOTER);

			-- STENCIL
			section_mark (section_stencil, HEADER);

			section_mark (section_top, HEADER);
				iterate (element (module_cursor).board.stencil.top.lines, write_line'access);
				iterate (element (module_cursor).board.stencil.top.arcs, write_arc'access);
				iterate (element (module_cursor).board.stencil.top.circles, write_circle'access);
				iterate (element (module_cursor).board.stencil.top.polygons, write_polygon'access);
				iterate (element (module_cursor).board.stencil.top.cutouts, write_cutout'access);
				section_mark (section_top, FOOTER);

				section_mark (section_bottom, HEADER);
				iterate (element (module_cursor).board.stencil.bottom.lines, write_line'access);
				iterate (element (module_cursor).board.stencil.bottom.arcs, write_arc'access);
				iterate (element (module_cursor).board.stencil.bottom.circles, write_circle'access);
				iterate (element (module_cursor).board.stencil.bottom.polygons, write_polygon'access);
				iterate (element (module_cursor).board.stencil.bottom.cutouts, write_cutout'access);
			section_mark (section_bottom, FOOTER);

			section_mark (section_stencil, FOOTER);

			-- STOP MASK
			section_mark (section_stop_mask, HEADER);

			section_mark (section_top, HEADER);
				iterate (element (module_cursor).board.stop_mask.top.lines, write_line'access);
				iterate (element (module_cursor).board.stop_mask.top.arcs, write_arc'access);
				iterate (element (module_cursor).board.stop_mask.top.circles, write_circle'access);
				iterate (element (module_cursor).board.stop_mask.top.polygons, write_polygon'access);
				iterate (element (module_cursor).board.stop_mask.top.cutouts, write_cutout'access);
				iterate (element (module_cursor).board.stop_mask.top.texts, write_text'access);			
				section_mark (section_top, FOOTER);

				section_mark (section_bottom, HEADER);
				iterate (element (module_cursor).board.stop_mask.bottom.lines, write_line'access);
				iterate (element (module_cursor).board.stop_mask.bottom.arcs, write_arc'access);
				iterate (element (module_cursor).board.stop_mask.bottom.circles, write_circle'access);
				iterate (element (module_cursor).board.stop_mask.bottom.polygons, write_polygon'access);
				iterate (element (module_cursor).board.stop_mask.bottom.cutouts, write_cutout'access);
				iterate (element (module_cursor).board.stop_mask.bottom.texts, write_text'access);
				section_mark (section_bottom, FOOTER);

			section_mark (section_stop_mask, FOOTER);

			-- KEEPOUT
			section_mark (section_keepout, HEADER);

			section_mark (section_top, HEADER);
				iterate (element (module_cursor).board.keepout.top.lines, write_line'access);
				iterate (element (module_cursor).board.keepout.top.arcs, write_arc'access);
				iterate (element (module_cursor).board.keepout.top.circles, write_circle'access);
				iterate (element (module_cursor).board.keepout.top.polygons, write_polygon'access);
				iterate (element (module_cursor).board.keepout.top.cutouts, write_cutout'access);
				-- CS iterate (et_schematic.element (module_cursor).board.keepout.top.texts, write_text'access);
				section_mark (section_top, FOOTER);

				section_mark (section_bottom, HEADER);
				iterate (element (module_cursor).board.keepout.bottom.lines, write_line'access);
				iterate (element (module_cursor).board.keepout.bottom.arcs, write_arc'access);
				iterate (element (module_cursor).board.keepout.bottom.circles, write_circle'access);
				iterate (element (module_cursor).board.keepout.bottom.polygons, write_polygon'access);
				iterate (element (module_cursor).board.keepout.bottom.cutouts, write_cutout'access);
				-- CS iterate (et_schematic.element (module_cursor).board.keepout.bottom.texts, write_text'access);
				section_mark (section_bottom, FOOTER);

			section_mark (section_keepout, FOOTER);

			-- ROUTE RESTRICT
			section_mark (section_route_restrict, HEADER);
				iterate (element (module_cursor).board.route_restrict.lines, write_line'access);
				iterate (element (module_cursor).board.route_restrict.arcs, write_arc'access);
				iterate (element (module_cursor).board.route_restrict.circles, write_circle'access);
				iterate (element (module_cursor).board.route_restrict.polygons, write_polygon'access);
				iterate (element (module_cursor).board.route_restrict.cutouts, write_cutout'access);
				-- CS iterate (et_schematic.element (module_cursor).board.route_restrict.texts, write_text'access);
			section_mark (section_route_restrict, FOOTER);

			-- VIA RESTRICT
			section_mark (section_via_restrict, HEADER);
				iterate (element (module_cursor).board.via_restrict.lines, write_line'access);
				iterate (element (module_cursor).board.via_restrict.arcs, write_arc'access);
				iterate (element (module_cursor).board.via_restrict.circles, write_circle'access);
				iterate (element (module_cursor).board.via_restrict.polygons, write_polygon'access);
				iterate (element (module_cursor).board.via_restrict.cutouts, write_cutout'access);
				-- CS iterate (et_schematic.element (module_cursor).board.via_restrict.texts, write_text'access);
			section_mark (section_via_restrict, FOOTER);

			-- COPPER (NON-ELECTRIC)
			section_mark (section_copper, HEADER);
				iterate (element (module_cursor).board.copper.lines, write_line'access);
				iterate (element (module_cursor).board.copper.arcs, write_arc'access);
				iterate (element (module_cursor).board.copper.circles, write_circle'access);
				iterate (element (module_cursor).board.copper.polygons.solid, write_polygon'access);
				iterate (element (module_cursor).board.copper.polygons.hatched, write_polygon'access);
				iterate (element (module_cursor).board.copper.cutouts, write_cutout'access);			
				iterate (element (module_cursor).board.copper.texts, write_text'access);
				iterate (element (module_cursor).board.copper.placeholders, write_placeholder'access);
			section_mark (section_copper, FOOTER);

			-- BOARD CONTOUR
			section_mark (section_pcb_contours, HEADER);
				iterate (element (module_cursor).board.contours.lines, write_line'access);
				iterate (element (module_cursor).board.contours.arcs, write_arc'access);
				iterate (element (module_cursor).board.contours.circles, write_circle'access);
			section_mark (section_pcb_contours, FOOTER);
			
			---BOARD END-----
			section_mark (section_board, FOOTER);
		end query_board;

	begin -- save_module
		write_header;

		-- meta data
		query_meta;
		put_line (row_separator_single);
		
		-- net classes
		query_net_classes;
		put_line (row_separator_single);

		-- drawing grid
		query_drawing_grid;
		put_line (row_separator_single);

		-- layer stack
		query_layer_stack;
		put_line (row_separator_single);
		
		-- nets
		query_nets;
		put_line (row_separator_single);
		
		-- frames
		query_frames;
		put_line (row_separator_single);
		
		-- notes
		query_texts;
		put_line (row_separator_single);
		
		-- submodules
		query_submodules;
		put_line (row_separator_single);
		
		-- devices
		query_devices;
		put_line (row_separator_single);

		-- assembly variants
		query_assembly_variants;
		put_line (row_separator_single);
		
		-- netchangers
		query_netchangers;
		put_line (row_separator_single);
		
		-- board
		query_board;
		put_line (row_separator_single);	

		write_footer;

		exception when event:
			others => 
				log (text => ada.exceptions.exception_message (event), console => true);
				close (module_file_handle);
				raise;
		
	end save_module;
	
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
		file_name 		: in string; -- motor_driver.mod, templates/clock_generator.mod
		log_threshold	: in et_string_processing.type_log_level) 
		is
		previous_input : ada.text_io.file_type renames current_input;

		use et_string_processing;

		-- This is the full file name with its path after expanding
		-- (environment variables could be in file name):
		full_file_name : constant string := expand (file_name);
			
		file_handle : ada.text_io.file_type;
		use type_modules;
		module_cursor : type_modules.cursor;
		module_inserted : boolean;

		line : et_string_processing.type_fields_of_line;

		-- This is the section stack of the module. 
		-- Here we track the sections. On entering a section, its name is
		-- pushed onto the stack. When leaving a section the latest section name is popped.
		max_section_depth : constant positive := 11;
		package stack is new general_rw.stack_lifo (
			item	=> type_section,
			max 	=> max_section_depth);


		-- META DATA
		meta_basic		: et_meta.type_basic;
		meta_schematic	: et_meta.type_schematic;
		meta_board		: et_meta.type_board;
		
		active_assembly_variant : et_general.type_variant_name.bounded_string; -- "low_cost"
		
		procedure set_active_assembly_variant is
		-- Assigns to the module the active assembly variant.
			use et_schematic;
			use assembly_variants;
			
			kw : constant string := f (line, 1);

			procedure set_variant (
				module_name	: in type_module_name.bounded_string;
				module		: in out type_module) is
			begin
				module.active_variant := active_assembly_variant;
			end;
			
		begin
			if kw = keyword_active then
				expect_field_count (line, 2);
				active_assembly_variant := to_variant (f (line, 2));
			else
				invalid_keyword (kw);
			end if;

			update_element (modules, module_cursor, set_variant'access);
		end;

		procedure set_meta is
		-- Assigns the collected meta data to the module.
			use et_schematic;
			procedure do_it (
				module_name	: in type_module_name.bounded_string;
				module		: in out type_module) is
			begin
				-- CS check whether date drawn <= date checked <= date_approved
				--  use type_basic for the test of schematic and board data.
				
				module.meta.schematic := meta_schematic;
				module.meta.board := meta_board;
			end;
		begin
			log (text => "meta data ...", level => log_threshold + 1);
			
			update_element (
				container	=> modules,
				position	=> module_cursor,
				process		=> do_it'access);
		end;

		function read_meta_basic return boolean is
		-- Reads basic meta data. If given line does not contain
		-- basic meta stuff, returns a false.
			use et_meta;
			kw : constant string := f (line, 1);
			result : boolean := true;
		begin
			if kw = keyword_company then
				expect_field_count (line, 2);
				meta_basic.company := to_company (f (line, 2));

			elsif kw = keyword_customer then
				expect_field_count (line, 2);
				meta_basic.customer := to_customer (f (line, 2));
				
			elsif kw = keyword_partcode then
				expect_field_count (line, 2);
				meta_basic.partcode := to_partcode (f (line, 2));
				
			elsif kw = keyword_drawing_number then
				expect_field_count (line, 2);
				meta_basic.drawing_number := to_drawing_number (f (line, 2));
				
			elsif kw = keyword_revision then
				expect_field_count (line, 2);
				meta_basic.revision := to_revision (f (line, 2));
				
			elsif kw = keyword_drawn_by then
				expect_field_count (line, 2);
				meta_basic.drawn_by := to_person (f (line, 2));
				
			elsif kw = keyword_drawn_date then
				expect_field_count (line, 2);
				meta_basic.drawn_date := to_date (f (line, 2));
				
			elsif kw = keyword_checked_by then
				expect_field_count (line, 2);
				meta_basic.checked_by := to_person (f (line, 2));
				
			elsif kw = keyword_checked_date then
				expect_field_count (line, 2);
				meta_basic.checked_date := to_date (f (line, 2));
				
			elsif kw = keyword_approved_by then
				expect_field_count (line, 2);
				meta_basic.approved_by := to_person (f (line, 2));
				
			elsif kw = keyword_approved_date then
				expect_field_count (line, 2);
				meta_basic.approved_date := to_date (f (line, 2));

			else
				result := false;
			end if;
			
			return result;
		end read_meta_basic;
			
		procedure read_meta_schematic is 
			use et_meta;
			kw : constant string := f (line, 1);
		begin
			-- first parse line for basic meta stuff.
			-- if no meta stuff found, test for schematic specific meta data:
			if read_meta_basic = false then
				-- CS: in the future, if there is schematic specific meta data:
				-- if kw = keyword_xyz then
				-- do something
				--else
				invalid_keyword (kw);
			end if;
		end;

		procedure read_meta_board is 
			use et_meta;			
			kw : constant string := f (line, 1);
		begin
			-- first parse line for basic meta stuff.
			-- if no meta stuff found, test for bord specific meta data:
			if read_meta_basic = false then
				-- CS: in the future, if there is schematic specific meta data:
				-- if kw = keyword_xyz then
				-- do something
				--else
				invalid_keyword (kw);
			end if;
		end;		
		
		function to_position (
			line : in type_fields_of_line; -- "position sheet 3 x 44.5 y 53.5"
			from : in positive)
			return et_coordinates.type_position is
			
			use et_coordinates;
			use geometry;
			
			point : et_coordinates.type_position; -- to be returned
			place : positive := from; -- the field being read from given line

			-- CS: flags to detect missing sheet, x or y
		begin
			while place <= positive (field_count (line)) loop

				-- We expect after "sheet" the sheet number
				if f (line, place) = keyword_sheet then
					set_sheet (point, to_sheet (f (line, place + 1)));
					
				-- We expect after the x the corresponding value for x
				elsif f (line, place) = keyword_x then
					--set_x (point, to_distance (f (line, place + 1)));
					set (X, to_distance (f (line, place + 1)), point);

				-- We expect after the y the corresponding value for y
				elsif f (line, place) = keyword_y then
					--set_y (point, to_distance (f (line, place + 1)));
					set (Y, to_distance (f (line, place + 1)), point);

				else
					invalid_keyword (f (line, place));
				end if;
					
				place := place + 2;
			end loop;
			
			return point;
		end to_position;

		function to_size (
			line : in type_fields_of_line; -- "size x 30 y 40"
			from : in positive)
			return submodules.type_submodule_size is
			use et_coordinates.geometry;
			
			size : submodules.type_submodule_size; -- to be returned
			place : positive := from; -- the field being read from given line

			-- CS: flags to detect missing x or y
		begin
			while place <= positive (field_count (line)) loop

				-- We expect after the x the corresponding value for x
				if f (line, place) = keyword_x then
					size.x := to_distance (f (line, place + 1));

				-- We expect after the y the corresponding value for y
				elsif f (line, place) = keyword_y then
					size.y := to_distance (f (line, place + 1));

				else
					invalid_keyword (f (line, place));
				end if;
					
				place := place + 2;
			end loop;
			
			return size;
		end to_size;

		function to_position (
		-- Returns a type_package_position in the layout.
			line : in type_fields_of_line; -- "position x 23 y 0.2 rotation 90.0 face top"
			from : in positive)
			return et_pcb_coordinates.type_package_position is
			use et_pcb_coordinates;
			use et_pcb_coordinates.geometry;
			
			point : type_package_position; -- to be returned
			place : positive := from; -- the field being read from given line

			-- CS: flags to detect missing sheet, x or y
		begin
			while place <= positive (field_count (line)) loop

				-- We expect after the x the corresponding value for x
				if f (line, place) = keyword_x then
					set (point => point, axis => X, value => to_distance (f (line, place + 1)));

				-- We expect after the y the corresponding value for y
				elsif f (line, place) = keyword_y then
					set (point => point, axis => Y, value => to_distance (f (line, place + 1)));

				-- We expect after "rotation" the corresponding value for the rotation
				elsif f (line, place) = keyword_rotation then
					set (point, to_rotation (f (line, place + 1)));

				-- We expect after "face" the actual face (top/bottom)
				elsif f (line, place) = keyword_face then
					set_face (position => point, face => to_face (f (line, place + 1)));
				else
					invalid_keyword (f (line, place));
				end if;
					
				place := place + 2;
			end loop;
			
			return point;
		end to_position;

		
		-- VARIABLES FOR TEMPORARILY STORAGE AND ASSOCIATED HOUSEKEEPING SUBPROGRAMS:

		-- drawing grid
		grid_schematic : et_coordinates.geometry.type_grid; -- CS rename to schematic_grid
		grid_board : et_pcb_coordinates.geometry.type_grid; -- CS rename to board_grid

		procedure read_drawing_grid_schematic is 
			kw : constant string := f (line, 1);
		begin
			-- CS: In the following: set a corresponding parameter-found-flag
			if kw = keyword_default then -- default x 1.00 y 1.00
				expect_field_count (line, 5);
				grid_schematic := to_grid (line, 2);
			else
				invalid_keyword (kw);
			end if;
		end;

		procedure read_drawing_grid_board is
			kw : constant string := f (line, 1);
		begin
			-- CS: In the following: set a corresponding parameter-found-flag
			if kw = keyword_default then -- default x 1.00 y 1.00
				expect_field_count (line, 5);
				grid_board := to_grid (line, 2);
			else
				invalid_keyword (kw);
			end if;
		end;

		
		-- net class
		net_class 		: et_pcb.type_net_class;
		net_class_name	: et_pcb.type_net_class_name.bounded_string;

		procedure reset_net_class is 
			use et_pcb;
		begin
			net_class_name := net_class_name_default;
			net_class := (others => <>);

			-- CS reset parameter-found-flags
		end reset_net_class;

		-- nets
		net_name	: type_net_name.bounded_string; -- motor_on_off
		net			: et_schematic.type_net;

		strands : et_schematic.type_strands.list;
		strand	: et_schematic.type_strand;
		net_segments : et_schematic.type_net_segments.list;
		net_segment	: et_schematic.type_net_segment;
		
		net_labels : et_schematic.type_net_labels.list;
		net_label : et_schematic.type_net_label_base;
		net_label_appearance : et_schematic.type_net_label_appearance := et_schematic.type_net_label_appearance'first;

		-- The net label direction is relevant if appearance is TAG:
		net_label_direction : et_schematic.type_net_label_direction := et_schematic.type_net_label_direction'first;
		
		net_device_port : et_schematic.type_port_device;
		net_device_ports : et_schematic.type_ports_device.set;

		net_submodule_port : et_schematic.type_port_submodule;
		net_submodule_ports : et_schematic.type_ports_submodule.set;

		net_netchanger_port : netlists.type_port_netchanger;
		net_netchanger_ports : netlists.type_ports_netchanger.set;
		
		route		: et_pcb.type_route;
		route_via	: et_pcb.type_via;

		
		frame_template_schematic	: et_frames.pac_template_name.bounded_string;	-- $ET_FRAMES/drawing_frame_version_1.frs
		-- CS frame_count_schematic		: et_coordinates.type_submodule_sheet_number := et_coordinates.type_submodule_sheet_number'first; -- 10 frames
		frame_template_board		: et_frames.pac_template_name.bounded_string;	-- $ET_FRAMES/drawing_frame_version_2.frb

		procedure read_frame_template_schematic is
		-- Reads the name of the schematic frame template.
			use et_frames;
			kw : constant string := f (line, 1);
		begin
			-- CS: In the following: set a corresponding parameter-found-flag
			if kw = keyword_template then -- template $ET_FRAMES/drawing_frame_version_1.frs
				expect_field_count (line, 2);
				frame_template_schematic := to_template_name (f (line, 2));
			else
				invalid_keyword (kw);
			end if;
		end;

		procedure read_frame_template_board is
		-- Reads the name of the board frame template.		
			use et_frames;
			kw : constant string := f (line, 1);
		begin
			-- CS: In the following: set a corresponding parameter-found-flag
			if kw = keyword_template then -- template $ET_FRAMES/drawing_frame_version_2.frb
				expect_field_count (line, 2);
				frame_template_board := to_template_name (f (line, 2));
			else
				invalid_keyword (kw);
			end if;
		end;

		
		-- submodules
		submodule_port			: submodules.type_submodule_port;
		submodule_port_name		: et_general.type_net_name.bounded_string; -- RESET
		submodule_ports			: submodules.type_submodule_ports.map;
		submodule_name 			: et_general.type_module_instance_name.bounded_string; -- MOT_DRV_3
		submodule				: submodules.type_submodule;

		note : et_schematic.type_text;

		-- The temporarily device will exist where "device" points at:
		device					: access et_schematic.type_device;
		
		device_name				: type_name; -- C12
		device_model			: type_device_model_file.bounded_string; -- ../libraries/transistor/pnp.dev
		device_value			: type_value.bounded_string; -- 470R
		device_appearance		: et_schematic.type_appearance_schematic;
		--device_unit				: et_schematic.type_unit;
		--device_unit_rotation	: et_coordinates.type_rotation := geometry.zero_rotation;
		device_unit_mirror		: et_schematic.type_mirror := et_schematic.NO;
		device_unit_name		: type_unit_name.bounded_string; -- GPIO_BANK_1
		device_unit_position	: et_coordinates.type_position; -- x,y,sheet,rotation

		-- assembly variants
		assembly_variant_name			: et_general.type_variant_name.bounded_string; -- low_cost
		assembly_variant_description	: assembly_variants.type_description; -- "variant without temp. sensor"
		assembly_variant_devices		: assembly_variants.type_devices.map;
		assembly_variant_submodules		: assembly_variants.type_submodules.map;
		
		-- temporarily collection of units:
		device_units			: et_schematic.type_units.map; -- PWR, A, B, ...
		
		device_partcode			: material.type_partcode.bounded_string;
		device_purpose			: et_devices.type_purpose.bounded_string;
		device_variant			: et_devices.type_variant_name.bounded_string; -- D, N
		device_position			: et_pcb_coordinates.type_package_position; -- incl. angle and face
		device_flipped			: et_pcb.type_flipped := et_pcb.flipped_default;

		-- These two variables assist when a particular placeholder is appended to the
		-- list of placholders in silk screen, assy doc and their top or bottom face:
		device_text_placeholder_position: et_pcb_coordinates.type_package_position := et_pcb_coordinates.placeholder_position_default; -- incl. rotation and face
		device_text_placeholder_layer	: et_packages.type_placeholder_package_layer := et_packages.type_placeholder_package_layer'first; -- silk_screen/assembly_documentation

		-- a single temporarily placeholder of a package
		device_text_placeholder		: et_packages.type_text_placeholder;

		-- the temporarily collection of placeholders of packages
		device_text_placeholders	: et_packages.type_text_placeholders; -- silk screen, assy doc, top, bottom

		-- temporarily placeholders of unit reference (IC12), value (7400) and purpose (clock buffer)
		unit_placeholder			: et_symbols.type_text_basic;
		unit_placeholder_position	: et_coordinates.geometry.type_point;
		unit_placeholder_meaning	: et_symbols.type_placeholder_meaning := et_symbols.placeholder_meaning_default;
		unit_placeholder_reference	: et_symbols.type_text_placeholder (meaning => et_symbols.NAME);
		unit_placeholder_value		: et_symbols.type_text_placeholder (meaning => et_symbols.VALUE);
		unit_placeholder_purpose	: et_symbols.type_text_placeholder (meaning => et_symbols.PURPOSE);

		-- temporarily a netchanger is stored here:
		netchanger		: submodules.type_netchanger;
		netchanger_id	: submodules.type_netchanger_id := submodules.type_netchanger_id'first;
					
		-- general board stuff
		board_text : et_packages.type_text_with_content;
		board_text_placeholder : et_pcb.type_text_placeholder;

		
		signal_layers : et_pcb_stack.type_signal_layers.set;
		conductor_layer, dielectric_layer : et_pcb_stack.type_signal_layer := et_pcb_stack.type_signal_layer'first;
		conductor_thickness : et_pcb_stack.type_conductor_thickness := et_pcb_stack.conductor_thickness_outer_default;
		dielectric_found : boolean := false;
		board_layer : et_pcb_stack.type_layer;
		board_layers : et_pcb_stack.package_layers.vector;
		
-- 		board_track_circle : et_pcb.type_copper_circle;
		board_text_copper : et_pcb.type_text;
		board_text_copper_placeholder : et_pcb.type_text_placeholder_copper;

		net_junctions : et_schematic.type_junctions;
		
		procedure set_junction (place : in string) is begin
			if f (line, 2) = keyword_start then
				net_junctions.start_point := true;
			end if;
			
			if f (line, 2) = keyword_end then
				net_junctions.end_point := true;
			end if;
		end set_junction;

		procedure read_layer is
			kw : constant string := f (line, 1);
			use et_pcb_stack;
			use package_layers;
			use et_pcb_coordinates.geometry;
		begin
			-- CS: In the following: set a corresponding parameter-found-flag
			if kw = keyword_conductor then -- conductor 1 0.035
				expect_field_count (line, 3);
				conductor_layer := to_signal_layer (f (line, 2));
				conductor_thickness := to_distance (f (line, 3));
				board_layer.conductor.thickness := conductor_thickness;

				-- Layer numbers must be continuous from top to bottom.
				-- After the dielectric of a layer the next conductor layer must
				-- have the next number:
				if dielectric_found then
					if to_index (board_layers.last) /= conductor_layer - 1 then
						log (ERROR, "expect conductor layer number" &
							to_string (to_index (board_layers.last) + 1) & " !",
							console => true);
						raise constraint_error;
					end if;
				end if;
				
				dielectric_found := false;

			elsif kw = keyword_dielectric then -- dielectric 1 1.5
				expect_field_count (line, 3);
				dielectric_layer := to_signal_layer (f (line, 2));
				board_layer.dielectric.thickness := to_distance (f (line, 3));
				dielectric_found := true;
				
				if dielectric_layer = conductor_layer then
					append (board_layers, board_layer);
				else
					log (ERROR, "expect dielectric layer number" & to_string (conductor_layer) & " !", console => true);
					raise constraint_error;
				end if;
			else
				invalid_keyword (kw);
			end if;
		end;
		

		procedure process_line is 

			procedure execute_section is
			-- Once a section concludes, the temporarily variables are read, evaluated
			-- and finally assembled to actual objects:
				
				-- for sorting general board stuff:
				type type_layer is (SILK_SCREEN, ASSEMBLY_DOCUMENTATION, STENCIL, STOP_MASK, KEEPOUT);
				
				procedure insert_net_class (
					module_name	: in type_module_name.bounded_string;
					module		: in out et_schematic.type_module) is
					use et_pcb;
					inserted : boolean;
					cursor : type_net_classes.cursor;
				begin -- insert_net_class
					log (text => "net class " & to_string (net_class_name), level => log_threshold + 1);

					-- CS: notify about missing parameters (by reading the parameter-found-flags)
					-- If a parameter is missing, the default is assumed. See type_net_class spec.
					
					type_net_classes.insert (
						container	=> module.net_classes,
						key			=> net_class_name,
						new_item	=> net_class,
						inserted	=> inserted,
						position	=> cursor);

					if not inserted then
						log (ERROR, "net class '" & et_pcb.to_string (net_class_name) 
								& "' already exists !", console => true);
						raise constraint_error;
					end if;

					reset_net_class; -- clean up for next net class
					
				end insert_net_class;

				procedure add_board_layer (
					module_name	: in type_module_name.bounded_string;
					module		: in out et_schematic.type_module) is
					use et_pcb_stack;
				begin
					log (text => "board layer stack", level => log_threshold + 1);

					-- Copy the collected layers (except the bottom conductor layer) into the module:
					module.board.stack.layers := board_layers;

					-- If the last entry was "conductor n t" then we assume that this
					-- was the bottom conductor layer (it does not have a dielectric layer underneath).
					if not dielectric_found then
						module.board.stack.bottom.thickness := conductor_thickness;
					else
						log (ERROR, "dielectric not allowed underneath the bottom conductor layer !", console => true);
						raise constraint_error;
					end if;
					
					-- reset layer values:
					dielectric_found := false;
					conductor_layer := et_pcb_stack.type_signal_layer'first;
					dielectric_layer := et_pcb_stack.type_signal_layer'first;
					conductor_thickness := et_pcb_stack.conductor_thickness_outer_default;
					board_layer := (others => <>);
					package_layers.clear (board_layers);

				end add_board_layer;
				
				procedure set_drawing_grid is

					procedure set (
						module_name	: in type_module_name.bounded_string;
						module		: in out et_schematic.type_module) is
						use et_coordinates.geometry;
						use et_pcb_coordinates.geometry;
					begin
						module.grid := grid_schematic;
						log (text => "schematic" & to_string (module.grid), level => log_threshold + 2);
						module.board.grid := grid_board;
						log (text => "board" & to_string (module.board.grid), level => log_threshold + 2);
					end;
					
				begin -- set_drawing_grid
					log (text => "drawing grid", level => log_threshold + 1);
					log_indentation_up;
					
					update_element (
						container	=> modules,
						position	=> module_cursor,
						process		=> set'access);

					log_indentation_down;
				end set_drawing_grid;
				
				procedure insert_net (
					module_name	: in type_module_name.bounded_string;
					module		: in out et_schematic.type_module) is
					use et_schematic;
					inserted : boolean;
					cursor : type_nets.cursor;
				begin -- insert_net
					log (text => "net " & et_general.to_string (net_name), level => log_threshold + 1);

					-- CS: notify about missing parameters (by reading the parameter-found-flags)
					-- If a parameter is missing, the default is assumed. See type_net spec.
					
					type_nets.insert (
						container	=> module.nets,
						key			=> net_name,
						new_item	=> net,
						inserted	=> inserted,
						position	=> cursor);

					if not inserted then
						log (ERROR, "net '" & et_general.to_string (net_name) 
							& "' already exists !", console => true);
						raise constraint_error;
					end if;

					-- clean up for next net
					net_name := to_net_name ("");
					net := (others => <>);
					
				end insert_net;
				
				procedure insert_submodule (
					module_name	: in type_module_name.bounded_string;
					module		: in out et_schematic.type_module) is
					use et_schematic;
					inserted : boolean;
					use submodules;
					use submodules.type_submodules;
					cursor : submodules.type_submodules.cursor;
				begin
					log (text => "submodule " & et_general.to_string (submodule_name), level => log_threshold + 1);

					-- CS: notify about missing parameters (by reading the parameter-found-flags)
					-- If a parameter is missing, the default is assumed. See type_submodule spec.
					
					type_submodules.insert (
						container	=> module.submods,
						key			=> submodule_name,	-- the instance name like MOT_DRV_3
						new_item	=> submodule,
						inserted	=> inserted,
						position	=> cursor);

					if not inserted then
						log (ERROR, "submodule '" & et_general.to_string (submodule_name) 
							& "' already exists !", console => true);
						raise constraint_error;
					end if;

					-- The submodule/template (kept in submodule.file) will be read later once the 
					-- parent module has been read completely.
					
					-- clean up for next submodule
					submodule_name := to_instance_name ("");
					submodule := (others => <>);
					
				end insert_submodule;
				
				procedure set_frame_schematic (
					module_name	: in type_module_name.bounded_string;
					module		: in out et_schematic.type_module) is
					use et_frames;
				begin
					log (text => "drawing frame schematic " & to_string (frame_template_schematic), level => log_threshold + 1);

					-- set the frame template name
					module.frames.template := frame_template_schematic;

					-- read the frame template file
					module.frames.frame := frame_rw.read_frame (
						file_name		=> frame_template_schematic,
						domain			=> SCHEMATIC,
						log_threshold	=> log_threshold + 2);
					
				end set_frame_schematic;
				
				procedure set_frame_board (
					module_name	: in type_module_name.bounded_string;
					module		: in out et_schematic.type_module) is
					use et_frames;
				begin
					log (text => "drawing frame board " & to_string (frame_template_board), level => log_threshold + 1);

					-- set the frame template name
					module.board.frame.template := frame_template_board;

					-- read the frame template file
					module.board.frame.frame := frame_rw.read_frame (
						file_name		=> frame_template_board,
						domain			=> PCB,
						log_threshold	=> log_threshold + 2);

				end set_frame_board;

				procedure insert_note (
					module_name	: in type_module_name.bounded_string;
					module		: in out et_schematic.type_module) is
				begin
					-- append note to collection of notes
					et_schematic.type_texts.append (module.texts, note);

					-- clean up for next note
					note := (others => <>);
				end insert_note;
				
				procedure insert_package_placeholder is
					use et_packages;
					use et_pcb_coordinates;
				begin
					device_text_placeholder.position := et_pcb_coordinates.geometry.type_position (device_text_placeholder_position);
					
					case device_text_placeholder_layer is
						when SILK_SCREEN => 
							case get_face (device_text_placeholder_position) is

								when TOP =>
									pac_text_placeholders.append (
										container	=> device_text_placeholders.silk_screen.top,
										new_item	=> device_text_placeholder);
									
								when BOTTOM =>
									pac_text_placeholders.append (
										container	=> device_text_placeholders.silk_screen.bottom,
										new_item	=> device_text_placeholder);
							end case;
							
						when ASSEMBLY_DOCUMENTATION =>
							case get_face (device_text_placeholder_position) is

								when TOP =>
									pac_text_placeholders.append (
										container	=> device_text_placeholders.assy_doc.top,
										new_item	=> device_text_placeholder);

								when BOTTOM =>
									pac_text_placeholders.append (
										container	=> device_text_placeholders.assy_doc.bottom,
										new_item	=> device_text_placeholder);
							end case;

					end case;

					-- reset placeholder for next placeholder
					device_text_placeholder := (others => <>);
					device_text_placeholder_position := placeholder_position_default;

				end insert_package_placeholder;

				procedure insert_unit is 
					use et_symbols;
				begin
					log_indentation_up;
					-- log (text => "unit " & to_string (device_unit_name), log_threshold + 1);
					-- No good idea. Confuses operator because units are collected BEFORE the device is complete.
					
					-- Depending on the appearance of the device, a virtual or real unit
					-- is inserted in the unit list of the device.
					
					case device_appearance is
						when VIRTUAL =>
							et_schematic.type_units.insert (
								container	=> device_units,
								key			=> device_unit_name,
								new_item	=> (
									appearance	=> VIRTUAL,
									mirror		=> device_unit_mirror,
									position	=> device_unit_position));
												   
						when PCB =>
							-- A unit of a real device has placeholders:
							et_schematic.type_units.insert (
								container	=> device_units,
								key			=> device_unit_name,
								new_item	=> (
									mirror		=> device_unit_mirror,

									position	=> device_unit_position,
									appearance	=> PCB,

									-- The placeholders for reference, value and purpose have
									-- been built and can now be assigned to the unit:
									name		=> unit_placeholder_reference,
									value 		=> unit_placeholder_value,
									purpose		=> unit_placeholder_purpose));
					end case;

					-- clean up for next unit
					device_unit_position := zero_position;
					device_unit_name := et_devices.unit_name_default;
					--device_unit := (others => <>);
					device_unit_mirror := et_schematic.NO;
					--device_unit_rotation := geometry.zero_rotation;

					-- CS reset placeholders for name, value and purpose ?

					log_indentation_down;
				end insert_unit;

				procedure build_unit_placeholder is
				-- Builds a placeholder from unit_placeholder_meaning, unit_placeholder_position and unit_placeholder.
				-- Depending on the meaning of the placeholder it becomes a placeholder 
				-- for the reference (like R4), the value (like 100R) or the purpose (like "brightness control").
					use et_symbols;
				begin
					case unit_placeholder_meaning is
						when NAME =>
							unit_placeholder_reference := (unit_placeholder with
								meaning		=> NAME,
								position	=> unit_placeholder_position);
							
						when VALUE =>
							unit_placeholder_value := (unit_placeholder with
								meaning		=> VALUE,
								position	=> unit_placeholder_position);

						when PURPOSE =>
							unit_placeholder_purpose := (unit_placeholder with
								meaning		=> PURPOSE,
								position	=> unit_placeholder_position);

						when others =>
							log (ERROR, "meaning of placeholder not supported !", console => true);
							raise constraint_error;
					end case;

					-- clean up for next placeholder
					unit_placeholder := (others => <>);
					unit_placeholder_meaning := placeholder_meaning_default;
					unit_placeholder_position := geometry.origin;
					
				end build_unit_placeholder;

				procedure insert_device (
					module_name	: in type_module_name.bounded_string;
					module		: in out et_schematic.type_module) is
					use et_schematic;
					use et_symbols;
					use et_devices;
					use et_packages;
					
					device_cursor : et_schematic.type_devices.cursor;
					inserted : boolean;

					function get_package_name return type_component_package_name.bounded_string is
					-- Derives package name from device.model and device.variant.
					-- Checks if variant exits in device.model.
						name : type_component_package_name.bounded_string; -- S_SO14 -- to be returned
						device_cursor : et_devices.type_devices.cursor;

						procedure query_variants (
							model	: in type_device_model_file.bounded_string; -- libraries/devices/7400.dev
							dev_lib	: in et_devices.type_device) -- a device in the library 
							is
							use pac_variants;
							variant_cursor : pac_variants.cursor;
							use ada.directories;
						begin -- query_variants
							-- Locate the variant (specified by the device in the module) in
							-- the device model.
							variant_cursor := pac_variants.find (
								container	=> dev_lib.variants,
								key			=> device.variant); -- the variant name from the module !

							-- The variant should be there. Otherwise abort.
							if variant_cursor = pac_variants.no_element then
								log (ERROR, "variant " & to_string (device.variant) &
									" not available in device model " & to_string (model) & " !", console => true);
								raise constraint_error;
							else
								name := to_package_name (base_name (et_packages.to_string (element (variant_cursor).package_model)));
							end if;
						end;
						
					begin -- get_package_name
						log_indentation_up;
						log (text => "verifying package variant " & to_string (device.variant) &
								" in device model " & to_string (device.model) & " ... ", level => log_threshold + 2);

						-- Locate the device in the library. CS: It should be there, otherwise exception arises here:
						device_cursor := et_devices.type_devices.find (
							container	=> et_devices.devices,
							key			=> device.model); -- libraries/devices/7400.dev

						-- Query package variants
						et_devices.type_devices.query_element (
							position	=> device_cursor,
							process		=> query_variants'access);
						
						log_indentation_down;
						return name;
					end get_package_name;
					
				begin -- insert_device
					log (text => "device " & to_string (device_name), level => log_threshold + 1);
					log_indentation_up;

					if not conventions.prefix_valid (device_name) then 
						--log (message_warning & "prefix of device " & et_libraries.to_string (device_name) 
						--	 & " not conformant with conventions !");
						null; -- CS output something helpful
					end if;
					
					-- assign temporarily variable for model:
					device.model := device_model;

					-- assign appearance specific temporarily variables and write log information
					if device.appearance = PCB then

						if not value_characters_valid (device_value) then
							log (WARNING, "value of " & to_string (device_name) &
								 " contains invalid characters !");
							log_indentation_reset;
							value_invalid (to_string (device_value));
						end if;
						
						log (text => "value " & to_string (device_value), level => log_threshold + 2);
						device.value := device_value;
						if not conventions.value_valid (device_value, prefix (device_name)) then
							log (WARNING, "value of " & to_string (device_name) &
								" not conformant with conventions !");
						end if;

						log (text => "partcode " & material.to_string (device_partcode), level => log_threshold + 2);
						if material.partcode_characters_valid (device_partcode) then
							device.partcode	:= device_partcode;
						else
							log_indentation_reset;
							material.partcode_invalid (material.to_string (device_partcode));
						end if;

						log (text => "purpose " & to_string (device_purpose), level => log_threshold + 2);
						if purpose_characters_valid (device_purpose) then
							device.purpose	:= device_purpose;
						else
							log_indentation_reset;
							purpose_invalid (to_string (device_purpose));
						end if;

						log (text => "variant " & to_string (device_variant), level => log_threshold + 2);
						check_variant_name_characters (device_variant);
						device.variant	:= device_variant;

						-- CS: warn operator if provided but ignored due to the fact that device is virtual
					end if;

					et_schematic.type_devices.insert (
						container	=> module.devices,
						position	=> device_cursor,
						inserted	=> inserted,
						key			=> device_name, -- IC23, R5, LED12
						new_item	=> device.all);

					if not inserted then
						log (ERROR, "device name " & to_string (device_name) & " already used !",
								console => true);
						raise constraint_error;
					end if;

					-- read the device model (like ../libraries/transistor/pnp.dev)
					read_device (device.model, log_threshold + 2);

					if device.appearance = PCB then
						conventions.validate_partcode (
							partcode		=> device.partcode,
							device_name		=> device_name,

							-- Derive package name from device.model and device.variant.
							-- Check if variant specified in device.model.
							packge			=> get_package_name, 
							
							value			=> device.value,
							log_threshold	=> log_threshold + 2);
					end if;
					
					-- reset pointer "device" so that the old device gets destroyed
					device := null;

					-- clean up temporarily variables for next device
					device_model	:= to_file_name ("");
					device_value	:= type_value.to_bounded_string ("");
					device_purpose	:= type_purpose.to_bounded_string ("");
					device_partcode := material.type_partcode.to_bounded_string ("");
					device_variant	:= to_name ("");

					log_indentation_down;
				end insert_device;						
								
				procedure insert_line (
					layer	: in type_layer; -- SILK_SCREEN, ASSEMBLY_DOCUMENTATION, ...
					face	: in et_pcb_coordinates.type_face) is -- TOP, BOTTOM
				-- The board_line and its board_line_width have been general things until now.
				-- Depending on the layer and the side of the board (face) the board_line
				-- is now assigned to the board where it belongs to.

					use et_packages;
					
					procedure do_it (
						module_name	: in type_module_name.bounded_string;
						module		: in out et_schematic.type_module) is
						use et_pcb_coordinates;
					begin -- do_it
						case face is
							when TOP =>
								case layer is
									when SILK_SCREEN =>
										type_silk_lines.append (
											container	=> module.board.silk_screen.top.lines,
											new_item	=> (pac_shapes.type_line (board_line) with board_line_width));

									when ASSEMBLY_DOCUMENTATION =>
										type_doc_lines.append (
											container	=> module.board.assy_doc.top.lines,
											new_item	=> (pac_shapes.type_line (board_line) with board_line_width));

									when STENCIL =>
										type_stencil_lines.append (
											container	=> module.board.stencil.top.lines,
											new_item	=> (pac_shapes.type_line (board_line) with board_line_width));
										
									when STOP_MASK =>
										type_stop_lines.append (
											container	=> module.board.stop_mask.top.lines,
											new_item	=> (pac_shapes.type_line (board_line) with board_line_width));

									when KEEPOUT =>
										type_keepout_lines.append (
											container	=> module.board.keepout.top.lines,
											new_item	=> (pac_shapes.type_line (board_line) with null record));
										
								end case;
								
							when BOTTOM => null;
								case layer is
									when SILK_SCREEN =>
										type_silk_lines.append (
											container	=> module.board.silk_screen.bottom.lines,
											new_item	=> (pac_shapes.type_line (board_line) with board_line_width));

									when ASSEMBLY_DOCUMENTATION =>
										type_doc_lines.append (
											container	=> module.board.assy_doc.bottom.lines,
											new_item	=> (pac_shapes.type_line (board_line) with board_line_width));
										
									when STENCIL =>
										type_stencil_lines.append (
											container	=> module.board.stencil.bottom.lines,
											new_item	=> (pac_shapes.type_line (board_line) with board_line_width));
										
									when STOP_MASK =>
										type_stop_lines.append (
											container	=> module.board.stop_mask.bottom.lines,
											new_item	=> (pac_shapes.type_line (board_line) with board_line_width));

									when KEEPOUT =>
										type_keepout_lines.append (
											container	=> module.board.keepout.bottom.lines,
											new_item	=> (pac_shapes.type_line (board_line) with null record));

								end case;
								
						end case;
					end do_it;
										
				begin -- insert_line
					update_element (
						container	=> modules,
						position	=> module_cursor,
						process		=> do_it'access);

					-- clean up for next board line
					board_reset_line;
					board_reset_line_width;
				end insert_line;

				procedure insert_arc (
					layer	: in type_layer; -- SILK_SCREEN, ASSEMBLY_DOCUMENTATION, ...
					face	: in et_pcb_coordinates.type_face) is -- TOP, BOTTOM
				-- The board_arc and its board_line_width have been general things until now. 
				-- Depending on the layer and the side of the board (face) the board_arc
				-- is now assigned to the board where it belongs to.

					use et_packages;
					
					procedure do_it (
						module_name	: in type_module_name.bounded_string;
						module		: in out et_schematic.type_module) is
						use et_pcb_coordinates;
					begin -- do_it
						case face is
							when TOP =>
								case layer is
									when SILK_SCREEN =>
										type_silk_arcs.append (
											container	=> module.board.silk_screen.top.arcs,
											new_item	=> (pac_shapes.type_arc (board_arc) with board_line_width));

									when ASSEMBLY_DOCUMENTATION =>
										type_doc_arcs.append (
											container	=> module.board.assy_doc.top.arcs,
											new_item	=> (pac_shapes.type_arc (board_arc) with board_line_width));

									when STENCIL =>
										type_stencil_arcs.append (
											container	=> module.board.stencil.top.arcs,
											new_item	=> (pac_shapes.type_arc (board_arc) with board_line_width));
										
									when STOP_MASK =>
										type_stop_arcs.append (
											container	=> module.board.stop_mask.top.arcs,
											new_item	=> (pac_shapes.type_arc (board_arc) with board_line_width));

									when KEEPOUT =>
										type_keepout_arcs.append (
											container	=> module.board.keepout.top.arcs,
											new_item	=> (pac_shapes.type_arc (board_arc) with null record));
								end case;
								
							when BOTTOM => null;
								case layer is
									when SILK_SCREEN =>
										type_silk_arcs.append (
											container	=> module.board.silk_screen.bottom.arcs,
											new_item	=> (pac_shapes.type_arc (board_arc) with board_line_width));

									when ASSEMBLY_DOCUMENTATION =>
										type_doc_arcs.append (
											container	=> module.board.assy_doc.bottom.arcs,
											new_item	=> (pac_shapes.type_arc (board_arc) with board_line_width));
										
									when STENCIL =>
										type_stencil_arcs.append (
											container	=> module.board.stencil.bottom.arcs,
											new_item	=> (pac_shapes.type_arc (board_arc) with board_line_width));
										
									when STOP_MASK =>
										type_stop_arcs.append (
											container	=> module.board.stop_mask.bottom.arcs,
											new_item	=> (pac_shapes.type_arc (board_arc) with board_line_width));

									when KEEPOUT =>
										type_keepout_arcs.append (
											container	=> module.board.keepout.bottom.arcs,
											new_item	=> (pac_shapes.type_arc (board_arc) with null record));
								end case;
								
						end case;
					end do_it;
										
				begin -- insert_arc
					update_element (
						container	=> modules,
						position	=> module_cursor,
						process		=> do_it'access);

					-- clean up for next board arc
					board_reset_arc;
					board_reset_line_width;
				end insert_arc;

				procedure insert_circle (
					layer	: in type_layer; -- SILK_SCREEN, ASSEMBLY_DOCUMENTATION, ...
					face	: in et_pcb_coordinates.type_face) is -- TOP, BOTTOM
				-- The board_circle has been a general thing until now. 
				-- Depending on the layer and the side of the board (face) the board_circle
				-- is now assigned to the board where it belongs to.

					use et_packages;
					
					procedure do_it (
						module_name	: in type_module_name.bounded_string;
						module		: in out et_schematic.type_module) is
						use et_pcb_coordinates;
					begin -- do_it
						case face is
							when TOP =>
								case layer is
									when SILK_SCREEN =>
										type_silk_circles.append (
											container	=> module.board.silk_screen.top.circles,
											new_item	=> board_make_fillable_circle);

									when ASSEMBLY_DOCUMENTATION =>
										type_doc_circles.append (
											container	=> module.board.assy_doc.top.circles,
											new_item	=> board_make_fillable_circle);

									when STENCIL =>
										type_stencil_circles.append (
											container	=> module.board.stencil.top.circles,
											new_item	=> board_make_fillable_circle);
										
									when STOP_MASK =>
										type_stop_circles.append (
											container	=> module.board.stop_mask.top.circles,
											new_item	=> board_make_fillable_circle);

									when KEEPOUT =>
										type_keepout_circles.append (
											container	=> module.board.keepout.top.circles,
											new_item	=> board_make_fillable_circle_solid);
								end case;
								
							when BOTTOM => null;
								case layer is
									when SILK_SCREEN =>
										type_silk_circles.append (
											container	=> module.board.silk_screen.bottom.circles,
											new_item	=> board_make_fillable_circle);

									when ASSEMBLY_DOCUMENTATION =>
										type_doc_circles.append (
											container	=> module.board.assy_doc.bottom.circles,
											new_item	=> board_make_fillable_circle);
										
									when STENCIL =>
										type_stencil_circles.append (
											container	=> module.board.stencil.bottom.circles,
											new_item	=> board_make_fillable_circle);
										
									when STOP_MASK =>
										type_stop_circles.append (
											container	=> module.board.stop_mask.bottom.circles,
											new_item	=> board_make_fillable_circle);

									when KEEPOUT =>
										type_keepout_circles.append (
											container	=> module.board.keepout.bottom.circles,
											new_item	=> board_make_fillable_circle_solid);
								end case;
								
						end case;
					end do_it;
										
				begin -- insert_circle
					update_element (
						container	=> modules,
						position	=> module_cursor,
						process		=> do_it'access);

					-- clean up for next board circle
					board_reset_circle_fillable;
				end insert_circle;

				procedure insert_polygon (
					layer	: in type_layer; -- SILK_SCREEN, ASSEMBLY_DOCUMENTATION, ...
					face	: in et_pcb_coordinates.type_face) is -- TOP, BOTTOM
				-- The polygon has been a general thing until now. 
				-- Depending on the layer and the side of the board (face) the polygon
				-- is now assigned to the board where it belongs to.

					procedure do_it (
						module_name	: in type_module_name.bounded_string;
						module		: in out et_schematic.type_module) is
						use et_pcb_coordinates;
						use et_packages;
						use et_packages.pac_shapes;
						
						procedure append_silk_polygon_top is begin
							case board_fill_style is 
								when SOLID =>
									pac_silk_polygons.append (
										container	=> module.board.silk_screen.top.polygons,
										new_item	=> (pac_shapes.type_polygon_base (polygon) with 
														fill_style 	=> SOLID,
														easing		=> board_easing));

								when HATCHED =>
									pac_silk_polygons.append (
										container	=> module.board.silk_screen.top.polygons,
										new_item	=> (pac_shapes.type_polygon_base (polygon) with 
														fill_style	=> HATCHED,
														easing		=> board_easing,
														hatching	=> board_hatching));
							end case;
						end;

						procedure append_silk_polygon_bottom is begin
							case board_fill_style is 
								when SOLID =>
									pac_silk_polygons.append (
										container	=> module.board.silk_screen.bottom.polygons,
										new_item	=> (pac_shapes.type_polygon_base (polygon) with 
														fill_style 	=> SOLID,
														easing		=> board_easing));

								when HATCHED =>
									pac_silk_polygons.append (
										container	=> module.board.silk_screen.bottom.polygons,
										new_item	=> (pac_shapes.type_polygon_base (polygon) with 
														fill_style	=> HATCHED,
														easing		=> board_easing,
														hatching	=> board_hatching));
							end case;
						end;
						
						procedure append_assy_doc_polygon_top is begin
							case board_fill_style is 
								when SOLID =>
									pac_doc_polygons.append (
										container	=> module.board.assy_doc.top.polygons,
										new_item	=> (pac_shapes.type_polygon_base (polygon) with 
														easing		=> board_easing,
														fill_style 	=> SOLID));

								when HATCHED =>
									pac_doc_polygons.append (
										container	=> module.board.assy_doc.top.polygons,
										new_item	=> (pac_shapes.type_polygon_base (polygon) with 
														fill_style	=> HATCHED,
														easing		=> board_easing,
														hatching	=> board_hatching));
							end case;
						end;

						procedure append_assy_doc_polygon_bottom is begin
							case board_fill_style is 
								when SOLID =>
									pac_doc_polygons.append (
										container	=> module.board.assy_doc.bottom.polygons,
										new_item	=> (pac_shapes.type_polygon_base (polygon) with 
														easing		=> board_easing,
														fill_style 	=> SOLID));

								when HATCHED =>
									pac_doc_polygons.append (
										container	=> module.board.assy_doc.bottom.polygons,
										new_item	=> (pac_shapes.type_polygon_base (polygon) with 
														fill_style	=> HATCHED,
														easing		=> board_easing,
														hatching	=> board_hatching));
							end case;
						end;

						procedure append_keepout_polygon_top is begin
							type_keepout_polygons.append (
								container	=> module.board.keepout.top.polygons, 
								new_item	=> (pac_shapes.type_polygon_base (polygon) with
												filled	=> board_filled));
						end;

						procedure append_keepout_polygon_bottom is begin
							type_keepout_polygons.append (
								container	=> module.board.keepout.bottom.polygons, 
								new_item	=> (pac_shapes.type_polygon_base (polygon) with
												filled	=> board_filled));
						end;

						procedure append_stencil_polygon_top is begin
							case board_fill_style is
								when SOLID =>
									type_stencil_polygons.append (
										container	=> module.board.stencil.top.polygons,
										new_item	=> (pac_shapes.type_polygon_base (polygon) with
														fill_style	=> SOLID,
														easing		=> board_easing));

								when HATCHED =>
									type_stencil_polygons.append (
										container	=> module.board.stencil.top.polygons,
										new_item	=> (pac_shapes.type_polygon_base (polygon) with
														fill_style	=> HATCHED,
														easing		=> board_easing,
														hatching	=> board_hatching));
							end case;
						end;

						procedure append_stencil_polygon_bottom is begin
							case board_fill_style is
								when SOLID =>
									type_stencil_polygons.append (
										container	=> module.board.stencil.bottom.polygons,
										new_item	=> (pac_shapes.type_polygon_base (polygon) with
														fill_style	=> SOLID,
														easing		=> board_easing));

								when HATCHED =>
									type_stencil_polygons.append (
										container	=> module.board.stencil.bottom.polygons,
										new_item	=> (pac_shapes.type_polygon_base (polygon) with
														fill_style	=> HATCHED,
														easing		=> board_easing,
														hatching	=> board_hatching));
							end case;
						end;

						procedure append_stop_polygon_top is begin
							case board_fill_style is
								when SOLID =>
									type_stop_polygons.append (
										container	=> module.board.stop_mask.top.polygons,
										new_item	=> (pac_shapes.type_polygon_base (polygon) with
														fill_style	=> SOLID,
														easing		=> board_easing));

								when HATCHED =>
									type_stop_polygons.append (
										container	=> module.board.stop_mask.top.polygons,
										new_item	=> (pac_shapes.type_polygon_base (polygon) with
														fill_style	=> HATCHED,
														easing		=> board_easing,
														hatching	=> board_hatching));
							end case;
						end;

						procedure append_stop_polygon_bottom is begin
							case board_fill_style is
								when SOLID =>
									type_stop_polygons.append (
										container	=> module.board.stop_mask.bottom.polygons,
										new_item	=> (pac_shapes.type_polygon_base (polygon) with
														fill_style	=> SOLID,
														easing		=> board_easing));

								when HATCHED =>
									type_stop_polygons.append (
										container	=> module.board.stop_mask.bottom.polygons,
										new_item	=> (pac_shapes.type_polygon_base (polygon) with
														fill_style	=> HATCHED,
														easing		=> board_easing,
														hatching	=> board_hatching));
							end case;
						end;
						
					begin -- do_it
						case face is
							when TOP =>
								case layer is
									when SILK_SCREEN =>
										append_silk_polygon_top;
													
									when ASSEMBLY_DOCUMENTATION =>
										append_assy_doc_polygon_top;

									when STENCIL =>
										append_stencil_polygon_top;
										
									when STOP_MASK =>
										append_stop_polygon_top;
										
									when KEEPOUT =>
										append_keepout_polygon_top;
										
								end case;
								
							when BOTTOM => null;
								case layer is
									when SILK_SCREEN =>
										append_silk_polygon_bottom;

									when ASSEMBLY_DOCUMENTATION =>
										append_assy_doc_polygon_bottom;
										
									when STENCIL =>
										append_stencil_polygon_bottom;
										
									when STOP_MASK =>
										append_stop_polygon_bottom;
										
									when KEEPOUT =>
										append_keepout_polygon_bottom;
										
								end case;
								
						end case;
					end do_it;
										
				begin -- insert_polygon
					update_element (
						container	=> modules,
						position	=> module_cursor,
						process		=> do_it'access);

					-- clean up for next board polygon
					board_reset_polygon;
				end insert_polygon;

				procedure insert_cutout (
					layer	: in type_layer; -- SILK_SCREEN, ASSEMBLY_DOCUMENTATION, ...
					face	: in et_pcb_coordinates.type_face) is -- TOP, BOTTOM
				-- The polygon has been a general thing until now. 
				-- Depending on the layer and the side of the board (face) the polygon
				-- is threated as a cutout zone and assigned to the board where it belongs to.

					procedure do_it (
						module_name	: in type_module_name.bounded_string;
						module		: in out et_schematic.type_module) is
						use et_pcb_coordinates;
						use et_packages;
						use et_packages.pac_shapes;
						
						procedure append_silk_cutout_top is begin
							pac_silk_cutouts.append (
								container	=> module.board.silk_screen.top.cutouts,
								new_item	=> (pac_shapes.type_polygon_base (polygon) with 
											easing		=> board_easing));
						end;

						procedure append_silk_cutout_bottom is begin
							pac_silk_cutouts.append (
								container	=> module.board.silk_screen.bottom.cutouts,
								new_item	=> (pac_shapes.type_polygon_base (polygon) with 
											easing		=> board_easing));
						end;
						
						procedure append_assy_doc_cutout_top is begin
							pac_doc_cutouts.append (
								container	=> module.board.assy_doc.top.cutouts,
								new_item	=> (pac_shapes.type_polygon_base (polygon) with 
												easing		=> board_easing));
						end;

						procedure append_assy_doc_cutout_bottom is begin
							pac_doc_cutouts.append (
								container	=> module.board.assy_doc.bottom.cutouts,
								new_item	=> (pac_shapes.type_polygon_base (polygon) with 
												easing		=> board_easing));
						end;

						procedure append_keepout_cutout_top is begin
							pac_keepout_cutouts.append (
								container	=> module.board.keepout.top.cutouts, 
								new_item	=> (pac_shapes.type_polygon_base (polygon) with
												easing		=> board_easing));
						end;

						procedure append_keepout_cutout_bottom is begin
							pac_keepout_cutouts.append (
								container	=> module.board.keepout.bottom.cutouts, 
								new_item	=> (pac_shapes.type_polygon_base (polygon) with
												easing		=> board_easing));
						end;

						procedure append_stencil_cutout_top is begin
							pac_stencil_cutouts.append (
								container	=> module.board.stencil.top.cutouts,
								new_item	=> (pac_shapes.type_polygon_base (polygon) with
												easing		=> board_easing));
						end;

						procedure append_stencil_cutout_bottom is begin
							pac_stencil_cutouts.append (
								container	=> module.board.stencil.bottom.cutouts,
								new_item	=> (pac_shapes.type_polygon_base (polygon) with
												easing		=> board_easing));
						end;

						procedure append_stop_cutout_top is begin
							pac_stop_cutouts.append (
								container	=> module.board.stop_mask.top.cutouts,
								new_item	=> (pac_shapes.type_polygon_base (polygon) with
												easing		=> board_easing));
						end;

						procedure append_stop_cutout_bottom is begin
							pac_stop_cutouts.append (
								container	=> module.board.stop_mask.bottom.cutouts,
								new_item	=> (pac_shapes.type_polygon_base (polygon) with
												easing		=> board_easing));
						end;
						
					begin -- do_it
						case face is
							when TOP =>
								case layer is
									when SILK_SCREEN =>
										append_silk_cutout_top;
													
									when ASSEMBLY_DOCUMENTATION =>
										append_assy_doc_cutout_top;

									when STENCIL =>
										append_stencil_cutout_top;
										
									when STOP_MASK =>
										append_stop_cutout_top;
										
									when KEEPOUT =>
										append_keepout_cutout_top;
										
								end case;
								
							when BOTTOM => null;
								case layer is
									when SILK_SCREEN =>
										append_silk_cutout_bottom;

									when ASSEMBLY_DOCUMENTATION =>
										append_assy_doc_cutout_bottom;
										
									when STENCIL =>
										append_stencil_cutout_bottom;
										
									when STOP_MASK =>
										append_stop_cutout_bottom;
										
									when KEEPOUT =>
										append_keepout_cutout_bottom;
										
								end case;
								
						end case;
					end do_it;
										
				begin -- insert_cutout
					update_element (
						container	=> modules,
						position	=> module_cursor,
						process		=> do_it'access);

					-- clean up for next board cutout
					board_reset_polygon;
				end insert_cutout;

				procedure insert_cutout_via_restrict is
					use et_packages;
					use et_pcb_stack;
					use type_signal_layers;
					
					procedure do_it (
						module_name	: in type_module_name.bounded_string;
						module		: in out et_schematic.type_module) is
					begin
						pac_via_restrict_cutouts.append (
							container	=> module.board.via_restrict.cutouts,
							new_item	=> (pac_shapes.type_polygon_base (polygon) with 
											easing	=> board_easing,
											layers	=> signal_layers));
					end do_it;
										
				begin
					update_element (
						container	=> modules,
						position	=> module_cursor,
						process		=> do_it'access);

					-- clean up for next board polygon
					board_reset_polygon;

					clear (signal_layers);
				end insert_cutout_via_restrict;

				procedure insert_cutout_route_restrict is
					use et_packages;
					use et_pcb_stack;
					use type_signal_layers;
					
					procedure do_it (
						module_name	: in type_module_name.bounded_string;
						module		: in out et_schematic.type_module) is
					begin
						pac_route_restrict_cutouts.append (
							container	=> module.board.route_restrict.cutouts,
							new_item	=> (pac_shapes.type_polygon_base (polygon) with 
											easing	=> board_easing,
											layers	=> signal_layers));
					end do_it;
										
				begin
					update_element (
						container	=> modules,
						position	=> module_cursor,
						process		=> do_it'access);

					-- clean up for next board polygon
					board_reset_polygon;

					clear (signal_layers);
				end insert_cutout_route_restrict;

				procedure insert_cutout_copper is
				-- This is about cutout zones to trim floating polygons in signal layers. No connection to any net.
					use et_packages;
					use et_packages.pac_shapes;
					use et_pcb;
					
					procedure do_it (
						module_name	: in type_module_name.bounded_string;
						module		: in out et_schematic.type_module) is
					begin
						et_pcb.pac_copper_cutouts.append (
							container	=> module.board.copper.cutouts,
							new_item	=> (type_polygon_base (polygon) with
									easing			=> board_easing,
									layer			=> signal_layer));
					end do_it;
										
				begin -- insert_cutout_copper
					update_element (
						container	=> modules,
						position	=> module_cursor,
						process		=> do_it'access);

					-- clean up for next floating board polygon
					board_reset_polygon;
				end insert_cutout_copper;
				
				procedure insert_text (
					layer	: in type_layer; -- SILK_SCREEN, ASSEMBLY_DOCUMENTATION, ...
					face	: in et_pcb_coordinates.type_face) is -- TOP, BOTTOM
				-- The board_text has been a general thing until now. 
				-- Depending on the layer and the side of the board (face) the board_text
				-- is now assigned to the board where it belongs to.
					
					procedure do_it (
						module_name	: in type_module_name.bounded_string;
						module		: in out et_schematic.type_module) is
						use et_pcb_coordinates;
						use et_packages;
						use et_pcb;
					begin -- do_it
						case face is
							when TOP =>
								case layer is
									when SILK_SCREEN =>
										type_texts_with_content.append (
											container	=> module.board.silk_screen.top.texts,
											new_item	=> board_text);

									when ASSEMBLY_DOCUMENTATION =>
										type_texts_with_content.append (
											container	=> module.board.assy_doc.top.texts,
											new_item	=> board_text);

									when STOP_MASK =>
										type_texts_with_content.append (
											container	=> module.board.stop_mask.top.texts,
											new_item	=> board_text);

									-- CS
									--when KEEPOUT =>
									--	type_texts_with_content.append (
									--		container	=> module.board.keepout.top.texts,
									--		new_item	=> board_text);

									when others => invalid_section;
								end case;
								
							when BOTTOM => null;
								case layer is
									when SILK_SCREEN =>
										type_texts_with_content.append (
											container	=> module.board.silk_screen.bottom.texts,
											new_item	=> board_text);

									when ASSEMBLY_DOCUMENTATION =>
										type_texts_with_content.append (
											container	=> module.board.assy_doc.bottom.texts,
											new_item	=> board_text);
										
									when STOP_MASK =>
										type_texts_with_content.append (
											container	=> module.board.stop_mask.bottom.texts,
											new_item	=> board_text);

									-- CS
									--when KEEPOUT =>
									--	type_texts_with_content.append (
									--		container	=> module.board.keepout.bottom.texts,
									--		new_item	=> board_text);

									when others => invalid_section;
								end case;
								
						end case;
					end do_it;
										
				begin -- insert_text
					update_element (
						container	=> modules,
						position	=> module_cursor,
						process		=> do_it'access);

					-- clean up for next board text
					board_text := (others => <>);
				end insert_text;

				procedure insert_placeholder (
					layer	: in type_layer; -- SILK_SCREEN, ASSEMBLY_DOCUMENTATION, ...
					face	: in et_pcb_coordinates.type_face) is -- TOP, BOTTOM
				-- The board_text_placeholder has been a general thing until now. 
				-- Depending on the layer and the side of the board (face) the board_text_placeholder
				-- is now assigned to the board where it belongs to.
					
					procedure do_it (
						module_name	: in type_module_name.bounded_string;
						module		: in out et_schematic.type_module) is
						use et_pcb_coordinates;
						use et_pcb;
					begin -- do_it
						case face is
							when TOP =>
								case layer is
									when SILK_SCREEN =>
										pac_text_placeholders.append (
											container	=> module.board.silk_screen.top.placeholders,
											new_item	=> board_text_placeholder);

									when ASSEMBLY_DOCUMENTATION =>
										pac_text_placeholders.append (
											container	=> module.board.assy_doc.top.placeholders,
											new_item	=> board_text_placeholder);

									when STOP_MASK =>
										pac_text_placeholders.append (
											container	=> module.board.stop_mask.top.placeholders,
											new_item	=> board_text_placeholder);

									-- CS
									--when KEEPOUT =>
									--	pac_text_placeholders.append (
									--		container	=> module.board.keepout.top.placeholders,
									--		new_item	=> board_text_placeholder);

									when others => invalid_section;
								end case;
								
							when BOTTOM => null;
								case layer is
									when SILK_SCREEN =>
										pac_text_placeholders.append (
											container	=> module.board.silk_screen.bottom.placeholders,
											new_item	=> board_text_placeholder);

									when ASSEMBLY_DOCUMENTATION =>
										pac_text_placeholders.append (
											container	=> module.board.assy_doc.bottom.placeholders,
											new_item	=> board_text_placeholder);
										
									when STOP_MASK =>
										pac_text_placeholders.append (
											container	=> module.board.stop_mask.bottom.placeholders,
											new_item	=> board_text_placeholder);

									-- CS
									--when KEEPOUT =>
									--	pac_text_placeholders.append (
									--		container	=> module.board.keepout.bottom.placeholders,
									--		new_item	=> board_text_placeholder);

									when others => invalid_section;
								end case;
								
						end case;
					end do_it;
										
				begin -- insert_placeholder
					update_element (
						container	=> modules,
						position	=> module_cursor,
						process		=> do_it'access);

					-- clean up for next board placeholder
					board_text_placeholder := (others => <>);
				end insert_placeholder;

				procedure insert_line_route_restrict is
					use et_packages;
					use et_pcb_stack;
					use type_signal_layers;
					
					procedure do_it (
						module_name	: in type_module_name.bounded_string;
						module		: in out et_schematic.type_module) is
					begin
						type_route_restrict_lines.append (
							container	=> module.board.route_restrict.lines,
							new_item	=> (pac_shapes.type_line (board_line) with 
											layers	=> signal_layers));
					end do_it;
										
				begin -- insert_line_route_restrict
					update_element (
						container	=> modules,
						position	=> module_cursor,
						process		=> do_it'access);

					-- clean up for next board line
					board_reset_line;
					clear (signal_layers);
				end insert_line_route_restrict;
				
				procedure insert_arc_route_restrict is
					use et_packages;
					use et_pcb_stack;					
					use type_signal_layers;
					
					procedure do_it (
						module_name	: in type_module_name.bounded_string;
						module		: in out et_schematic.type_module) is
					begin
						type_route_restrict_arcs.append (
							container	=> module.board.route_restrict.arcs,
							new_item	=> (pac_shapes.type_arc (board_arc) with 
											layers	=> signal_layers));
					end do_it;
										
				begin -- insert_arc_route_restrict
					update_element (
						container	=> modules,
						position	=> module_cursor,
						process		=> do_it'access);

					-- clean up for next board line
					board_reset_arc;

					clear (signal_layers);
				end insert_arc_route_restrict;

				procedure insert_circle_route_restrict is
					use et_packages;
					use et_pcb_stack;
					use type_signal_layers;
					
					procedure do_it (
						module_name	: in type_module_name.bounded_string;
						module		: in out et_schematic.type_module) is
					begin
						type_route_restrict_circles.append (
							container	=> module.board.route_restrict.circles,
							new_item	=> (board_make_fillable_circle_solid with signal_layers));
					end do_it;
										
				begin -- insert_circle_route_restrict
					update_element (
						container	=> modules,
						position	=> module_cursor,
						process		=> do_it'access);

					-- clean up for next board line
					board_reset_circle;
					clear (signal_layers);
				end insert_circle_route_restrict;

				procedure insert_polygon_route_restrict is
					use et_packages;
					use et_pcb_stack;
					use type_signal_layers;
					
					procedure do_it (
						module_name	: in type_module_name.bounded_string;
						module		: in out et_schematic.type_module) is
					begin
						type_route_restrict_polygons.append (
							container	=> module.board.route_restrict.polygons,
							new_item	=> (pac_shapes.type_polygon_base (polygon) with 
											filled	=> board_filled,
											layers	=> signal_layers));
					end do_it;
										
				begin -- insert_polygon_route_restrict
					update_element (
						container	=> modules,
						position	=> module_cursor,
						process		=> do_it'access);

					-- clean up for next board polygon
					board_reset_polygon;

					clear (signal_layers);
				end insert_polygon_route_restrict;

				procedure insert_line_via_restrict is
					use et_packages;
					use et_pcb_stack;
					use type_signal_layers;
					
					procedure do_it (
						module_name	: in type_module_name.bounded_string;
						module		: in out et_schematic.type_module) is
					begin
						type_via_restrict_lines.append (
							container	=> module.board.via_restrict.lines,
							new_item	=> (pac_shapes.type_line (board_line) with 
											layers	=> signal_layers));
					end do_it;
										
				begin -- insert_line_via_restrict
					update_element (
						container	=> modules,
						position	=> module_cursor,
						process		=> do_it'access);

					-- clean up for next board line
					board_reset_line;

					clear (signal_layers);
				end insert_line_via_restrict;

				procedure insert_arc_via_restrict is
					use et_packages;
					use et_pcb_stack;
					use type_signal_layers;
					
					procedure do_it (
						module_name	: in type_module_name.bounded_string;
						module		: in out et_schematic.type_module) is
					begin
						type_via_restrict_arcs.append (
							container	=> module.board.via_restrict.arcs,
							new_item	=> (pac_shapes.type_arc (board_arc) with 
											layers	=> signal_layers));
					end do_it;
										
				begin -- insert_arc_via_restrict
					update_element (
						container	=> modules,
						position	=> module_cursor,
						process		=> do_it'access);

					-- clean up for next board line
					board_reset_arc;

					clear (signal_layers);
				end insert_arc_via_restrict;

				procedure insert_circle_via_restrict is
					use et_packages;
					use et_pcb_stack;
					use type_signal_layers;
					
					procedure do_it (
						module_name	: in type_module_name.bounded_string;
						module		: in out et_schematic.type_module) is
					begin
						type_via_restrict_circles.append (
							container	=> module.board.via_restrict.circles,
							new_item	=> (board_make_fillable_circle_solid with signal_layers));
					end do_it;
										
				begin -- insert_circle_via_restrict
					update_element (
						container	=> modules,
						position	=> module_cursor,
						process		=> do_it'access);

					-- clean up for next board line
					board_reset_circle;

					clear (signal_layers);
				end insert_circle_via_restrict;

				procedure insert_polygon_via_restrict is
					use et_packages;
					use et_pcb_stack;
					use type_signal_layers;
					
					procedure do_it (
						module_name	: in type_module_name.bounded_string;
						module		: in out et_schematic.type_module) is
					begin
						type_via_restrict_polygons.append (
							container	=> module.board.via_restrict.polygons,
							new_item	=> (pac_shapes.type_polygon_base (polygon) with 
											filled	=> board_filled,
											layers	=> signal_layers));
					end do_it;
										
				begin -- insert_polygon_via_restrict
					update_element (
						container	=> modules,
						position	=> module_cursor,
						process		=> do_it'access);

					-- clean up for next board polygon
					board_reset_polygon;

					clear (signal_layers);
				end insert_polygon_via_restrict;

				procedure insert_polygon_copper is
				-- This is about floating polygons in signal layers. No connection to any net.
					use et_packages;
					use et_packages.pac_shapes;
					use et_pcb;
					
					procedure do_it (
						module_name	: in type_module_name.bounded_string;
						module		: in out et_schematic.type_module) is
					begin
						case board_fill_style is
							when SOLID =>
								pac_copper_polygons_floating_solid.append (
									container	=> module.board.copper.polygons.solid,
									new_item	=> (type_polygon_base (polygon) with
											fill_style 		=> SOLID,
											easing			=> board_easing,
											priority_level	=> polygon_priority,
											isolation		=> polygon_isolation,
											layer			=> signal_layer,
											width_min		=> polygon_width_min)
											);

							when HATCHED =>
								pac_copper_polygons_floating_hatched.append (
									container	=> module.board.copper.polygons.hatched,
									new_item	=> (type_polygon_base (polygon) with
											fill_style 		=> HATCHED,
											easing			=> board_easing,
											priority_level	=> polygon_priority,
											isolation		=> polygon_isolation,
											layer			=> signal_layer,
											width_min		=> polygon_width_min,
											hatching		=> board_hatching)
											);
						end case;
					end do_it;
										
				begin -- insert_polygon_copper
					update_element (
						container	=> modules,
						position	=> module_cursor,
						process		=> do_it'access);

					-- clean up for next floating board polygon
					board_reset_polygon;
				end insert_polygon_copper;

				procedure insert_line_track is -- about freetracks
					use et_pcb;
					
					procedure do_it (
						module_name	: in type_module_name.bounded_string;
						module		: in out et_schematic.type_module) is
					begin
						pac_copper_lines.append (
							container	=> module.board.copper.lines,
							new_item	=> (et_packages.pac_shapes.type_line (board_line) with
											width	=> board_line_width,
											layer	=> signal_layer));
					end;
										
				begin -- insert_line_track
					update_element (
						container	=> modules,
						position	=> module_cursor,
						process		=> do_it'access);

					-- clean up for next track line
					board_reset_line;
					board_reset_line_width;
					board_reset_signal_layer;
				end insert_line_track;

				procedure insert_arc_track is -- about freetracks
					use et_pcb;
					
					procedure do_it (
						module_name	: in type_module_name.bounded_string;
						module		: in out et_schematic.type_module) is
					begin
						pac_copper_arcs.append (
							container	=> module.board.copper.arcs,
							new_item	=> (et_packages.pac_shapes.type_arc (board_arc) with
											width	=> board_line_width,
											layer	=> signal_layer));
					end;
										
				begin -- insert_arc_track
					update_element (
						container	=> modules,
						position	=> module_cursor,
						process		=> do_it'access);

					-- clean up for next track arc
					board_reset_arc;
					board_reset_line_width;
					board_reset_signal_layer;
				end insert_arc_track;

				procedure insert_circle_track is -- about freetracks
					use et_pcb;
					
					procedure do_it (
						module_name	: in type_module_name.bounded_string;
						module		: in out et_schematic.type_module) is
					begin
						pac_copper_circles.append (
							container	=> module.board.copper.circles,
							new_item	=> (board_make_copper_circle with signal_layer));
					end;
										
				begin -- insert_circle_track
					update_element (
						container	=> modules,
						position	=> module_cursor,
						process		=> do_it'access);

					-- clean up for next track circle
					board_reset_circle;
					board_reset_line_width;
					board_reset_signal_layer;
					-- CS reset other properites
				end insert_circle_track;

				procedure insert_board_text is
					use et_pcb;
					
					procedure do_it (
						module_name	: in type_module_name.bounded_string;
						module		: in out et_schematic.type_module) is
					begin
						pac_texts.append (
							container	=> module.board.copper.texts,
							new_item	=> board_text_copper);
					end do_it;
										
				begin -- insert_board_text
					update_element (
						container	=> modules,
						position	=> module_cursor,
						process		=> do_it'access);

					-- clean up for next text in copper
					board_text_copper := (others => <>);
				end insert_board_text;

				procedure insert_board_text_placeholder is
					use et_pcb;
					
					procedure do_it (
						module_name	: in type_module_name.bounded_string;
						module		: in out et_schematic.type_module) is
					begin
						type_text_placeholders_copper.append (
							container	=> module.board.copper.placeholders,
							new_item	=> board_text_copper_placeholder);
					end do_it;
										
				begin -- insert_board_text_placeholder
					update_element (
						container	=> modules,
						position	=> module_cursor,
						process		=> do_it'access);

					-- clean up for next placeholder in copper
					board_text_copper_placeholder := (others => <>);
				end insert_board_text_placeholder;
				
				procedure insert_line_contour is
					use et_pcb;
					
					procedure do_it (
						module_name	: in type_module_name.bounded_string;
						module		: in out et_schematic.type_module) is
					begin
						type_pcb_contour_lines.append (
							container	=> module.board.contours.lines,
							new_item	=> (et_packages.pac_shapes.type_line (board_line) with board_lock_status));
					end do_it;
										
				begin -- insert_line_contour
					update_element (
						container	=> modules,
						position	=> module_cursor,
						process		=> do_it'access);

					-- clean up for next pcb contour line
					board_reset_line;
					board_reset_lock_status;
				end insert_line_contour;
				
				procedure insert_arc_contour is
					use et_pcb;
					
					procedure do_it (
						module_name	: in type_module_name.bounded_string;
						module		: in out et_schematic.type_module) is
					begin
						type_pcb_contour_arcs.append (
							container	=> module.board.contours.arcs,
							new_item	=> (et_packages.pac_shapes.type_arc (board_arc) with board_lock_status));
					end do_it;
										
				begin -- insert_arc_contour
					update_element (
						container	=> modules,
						position	=> module_cursor,
						process		=> do_it'access);

					-- clean up for next pcb contour arc
					board_reset_arc;
					board_reset_lock_status;
				end insert_arc_contour;

				procedure insert_circle_contour is
					use et_pcb;
					
					procedure do_it (
						module_name	: in type_module_name.bounded_string;
						module		: in out et_schematic.type_module) is
					begin
						type_pcb_contour_circles.append (
							container	=> module.board.contours.circles,
							new_item	=> (et_packages.pac_shapes.type_circle (board_circle) with board_lock_status));
					end do_it;
										
				begin -- insert_circle_contour
					update_element (
						container	=> modules,
						position	=> module_cursor,
						process		=> do_it'access);

					-- clean up for next pcb contour circle
					board_reset_circle;
					board_reset_lock_status;
				end insert_circle_contour;

				procedure insert_netchanger (
					module_name	: in type_module_name.bounded_string;
					module		: in out et_schematic.type_module) is
					inserted : boolean;
					use submodules;
					use type_netchangers;
					cursor : type_netchangers.cursor;
				begin
					log (text => "netchanger " & to_string (netchanger_id), level => log_threshold + 2);

					-- insert netchanger in container netchangers:
					insert (
						container	=> module.netchangers,
						key			=> netchanger_id,
						new_item	=> netchanger,
						inserted	=> inserted,
						position	=> cursor);

					-- A netchanger name must be unique:
					if not inserted then
						log (ERROR, "netchanger id" & to_string (netchanger_id) 
							& " already used !", console => true);
						raise constraint_error;
					end if;
					
					-- clean up for next netchanger
					netchanger_id := type_netchanger_id'first;
					netchanger := (others => <>);
				end insert_netchanger;

				procedure insert_assembly_variant (
					module_name	: in type_module_name.bounded_string;
					module		: in out et_schematic.type_module) is
					inserted : boolean;
					use assembly_variants;
					use assembly_variants.pac_variants;
					cursor : assembly_variants.pac_variants.cursor;
				begin
					log (text => "assembly variant " & 
						 enclose_in_quotes (to_variant (assembly_variant_name)), level => log_threshold + 2);

					-- insert variant in container variants
					assembly_variants.pac_variants.insert (
						container	=> module.variants,
						key			=> assembly_variant_name,
						inserted	=> inserted,
						position	=> cursor,
						new_item	=> (
							description	=> assembly_variant_description,
							devices		=> assembly_variant_devices,
							submodules	=> assembly_variant_submodules));

					-- An assembly variant must be unique:
					if not inserted then
						log (ERROR, "assembly variant " & 
							 enclose_in_quotes (to_variant (assembly_variant_name)) 
								& " already used !", console => true);
						raise constraint_error;
					end if;

					-- clean up for next assembly variant
					assembly_variant_name := to_variant ("");
					assembly_variant_description := to_unbounded_string ("");
					assembly_variant_devices := assembly_variants.type_devices.empty_map;
					assembly_variant_submodules := type_submodules.empty_map;
					
				end insert_assembly_variant;

				procedure build_route_polygon is
					use et_packages.pac_shapes;

					procedure solid_polygon is
						use et_pcb.pac_signal_polygons_solid;
						use et_packages;

						procedure connection_thermal is
							p : et_pcb.type_copper_polygon_solid (connection => et_pcb.THERMAL);
						begin
							p.segments := polygon.segments;
							
							p.easing := board_easing;
							
							p.width_min	:= polygon_width_min;
							p.isolation	:= polygon_isolation;
							
							p.layer				:= signal_layer;
							p.priority_level	:= polygon_priority;
							p.thermal			:= thermal;

							et_pcb.pac_signal_polygons_solid.append (
								container	=> route.polygons_2.solid,
								new_item	=> p);
						end;

						procedure connection_solid is
							p : et_pcb.type_copper_polygon_solid (connection => et_pcb.SOLID);
						begin
							p.segments := polygon.segments;
							
							p.easing := board_easing;
							
							p.width_min	:= polygon_width_min;
							p.isolation	:= polygon_isolation;
							
							p.layer				:= signal_layer;
							p.priority_level	:= polygon_priority;
							
							p.technology := thermal.technology;

							et_pcb.pac_signal_polygons_solid.append (
								container	=> route.polygons_2.solid,
								new_item	=> p);
						end;
						
					begin -- solid_polygon
						case polygon_pad_connection is
							when et_pcb.THERMAL => connection_thermal;
							when et_pcb.SOLID => connection_solid;
						end case;
					end solid_polygon;

					procedure hatched_polygon is
						use et_pcb.pac_signal_polygons_hatched;
						use et_packages;

						procedure connection_thermal is
							p : et_pcb.type_copper_polygon_hatched (connection => et_pcb.THERMAL);
						begin
							p.segments := polygon.segments;
							
							p.easing := board_easing;
							
							p.width_min	:= polygon_width_min;
							p.isolation	:= polygon_isolation;
							
							p.layer				:= signal_layer;
							p.priority_level	:= polygon_priority;
							p.thermal			:= thermal;
							
							et_pcb.pac_signal_polygons_hatched.append (
								container	=> route.polygons_2.hatched,
								new_item	=> p);
						end;

						procedure connection_solid is
							p : et_pcb.type_copper_polygon_hatched (connection => et_pcb.SOLID);
						begin
							p.segments := polygon.segments;
							
							p.easing := board_easing;
							
							p.width_min	:= polygon_width_min;
							p.isolation	:= polygon_isolation;
							
							p.layer				:= signal_layer;
							p.priority_level	:= polygon_priority;
							
							p.technology := thermal.technology;
							
							et_pcb.pac_signal_polygons_hatched.append (
								container	=> route.polygons_2.hatched,
								new_item	=> p);
						end;
						
					begin -- hatched_polygon
						case polygon_pad_connection is
							when et_pcb.THERMAL => connection_thermal;
							when et_pcb.SOLID => connection_solid;
						end case;
					end hatched_polygon;

				begin -- build_route_polygon
					case board_fill_style is
						when et_packages.SOLID		=> solid_polygon;
						when et_packages.HATCHED	=> hatched_polygon;
					end case;

					board_reset_polygon; -- clean up for next polygon
				end build_route_polygon;

				procedure build_route_cutout is
					use et_packages;
				begin
					et_pcb.pac_copper_cutouts.append (
						container	=> route.cutouts,
						new_item	=> (pac_shapes.type_polygon_base (polygon) with
										easing	=> board_easing,
										layer	=> signal_layer));

					board_reset_polygon; -- clean up for next cutout zone
				end;
				
			begin -- execute_section
				case stack.current is

					when SEC_CONTOURS =>
						case stack.parent is
							when SEC_CUTOUT_ZONE => null;
							when SEC_FILL_ZONE => null;
							when others => invalid_section;
						end case;
					
					when SEC_NET_CLASS =>
						case stack.parent is
							when SEC_NET_CLASSES =>

								-- insert net class
								update_element (
									container	=> modules,
									position	=> module_cursor,
									process		=> insert_net_class'access);
								
							when others => invalid_section;
						end case;

					when SEC_NET_CLASSES =>
						case stack.parent is
							when SEC_INIT => null;
							when others => invalid_section;
						end case;

					when SEC_BOARD_LAYER_STACK =>
						case stack.parent is
							when SEC_INIT => 

								-- add board layer
								update_element (
									container	=> modules,
									position	=> module_cursor,
									process		=> add_board_layer'access);
								
							when others => invalid_section;
						end case;
						
					when SEC_DRAWING_GRID =>
						case stack.parent is
							when SEC_INIT => set_drawing_grid;
							when others => invalid_section;
						end case;
						
					when SEC_NET =>
						case stack.parent is
							when SEC_NETS =>

								-- insert net
								update_element (
									container	=> modules,
									position	=> module_cursor,
									process		=> insert_net'access);

							when others => invalid_section;
						end case;

					when SEC_NETS =>
						case stack.parent is
							when SEC_INIT => null;
							when others => invalid_section;
						end case;
						
					when SEC_STRANDS =>
						case stack.parent is
							when SEC_NET =>

								-- insert strand collection in net
								net.strands := strands;
								et_schematic.type_strands.clear (strands); -- clean up for next strand collection

							when others => invalid_section;
						end case;

					when SEC_ROUTE =>
						case stack.parent is
							when SEC_NET =>

								-- insert route in net
								net.route := route;
								route := (others => <>); -- clean up route for next net
								
							when others => invalid_section;
						end case;

					when SEC_STRAND =>
						case stack.parent is
							when SEC_STRANDS =>

								declare
									use et_coordinates;
									use geometry;
									position_found_in_module_file : type_point := type_point (strand.position);
								begin
									-- Calculate the lowest x/y position and set sheet number of the strand
									-- and overwrite previous x/y position. 
									-- So the calculated position takes precedence over the position found in 
									-- the module file.
									et_schematic.set_strand_position (strand);

									-- Issue warning about this mismatch:
									if type_point (strand.position) /= position_found_in_module_file then
										log (WARNING, affected_line (line) & "Net " &
											 et_general.to_string (net_name) & ": Lowest x/y position of strand invalid !");
										log (text => " Found " & to_string (point => position_found_in_module_file));
										log (text => " Will be overridden by calculated position" & 
											 to_string (point => type_point (strand.position)));
									end if;
								end;
								
								-- insert strand in collection of strands
								et_schematic.type_strands.append (
									container	=> strands,
									new_item	=> strand);

								-- clean up for next single strand
								strand := (others => <>); 
								
							when others => invalid_section;
						end case;

					when SEC_SEGMENTS =>
						case stack.parent is
							when SEC_STRAND =>

								-- insert segments in strand
								strand.segments := net_segments;

								-- clean up for next segment collection
								et_schematic.type_net_segments.clear (net_segments);
								
							when others => invalid_section;
						end case;

					when SEC_SEGMENT =>
						case stack.parent is
							when SEC_SEGMENTS =>

								-- Copy the net_junctions into the segment.
								net_segment.junctions := net_junctions;

								-- Reset net_junctions for next net segment.
								net_junctions := (others => <>);
								
								-- insert segment in segment collection
								et_schematic.type_net_segments.append (
									container	=> net_segments,
									new_item	=> net_segment);

								-- clean up for next segment
								net_segment := (others => <>);
								
							when others => invalid_section;
						end case;

					when SEC_LABELS =>
						case stack.parent is
							when SEC_SEGMENT =>

								-- insert labels in segment
								net_segment.labels := net_labels;

								-- clean up for next label collection
								et_schematic.type_net_labels.clear (net_labels);

							when others => invalid_section;
						end case;

					when SEC_PORTS =>
						case stack.parent is
							when SEC_SEGMENT =>

								-- NOTE: A device, submodule or netchanger port is defined by a
								-- single line.
								-- Upon reading the a line like 
								--   "device/submodule/netchanger x port 1/4/slave/master" 
								-- the port is appended to the corresponding port collection 
								-- immediately when the line is read. See main code of process_line.
								-- There is no section for a single port like [PORT BEGIN].

								-- insert port collection in segment
								net_segment.ports_devices := net_device_ports;

								-- insert submodule ports in segment
								net_segment.ports_submodules := net_submodule_ports;

								-- insert netchanger ports in segment
								net_segment.ports_netchangers := net_netchanger_ports;
								
								-- clean up for next port collections (of another net segment)
								et_schematic.type_ports_device.clear (net_device_ports);
								et_schematic.type_ports_submodule.clear (net_submodule_ports);
								netlists.type_ports_netchanger.clear (net_netchanger_ports);

							when SEC_SUBMODULE =>
								-- copy collection of ports to submodule
								submodule.ports := submodule_ports;

								-- clean up for next collection of ports
								submodules.type_submodule_ports.clear (submodule_ports);
								
							when others => invalid_section;
						end case;
								
					when SEC_LABEL =>
						case stack.parent is
							when SEC_LABELS =>

								-- insert label in label collection
								case net_label_appearance is
									when et_schematic.SIMPLE =>

										-- insert a simple label
										et_schematic.type_net_labels.append (
											container	=> net_labels,
											new_item	=> (
												net_label with 
												appearance => et_schematic.SIMPLE));

										-- CS warn about parameter "direction" being ignored
										
									when et_schematic.TAG =>

										-- insert a tag label
										et_schematic.type_net_labels.append (
											container	=> net_labels,
											new_item	=> (
												net_label with 
												appearance	=> et_schematic.TAG,
												direction	=> net_label_direction));

								end case;

								-- clean up for next label
								net_label := (others => <>);
								net_label_appearance := et_schematic.type_net_label_appearance'first;
								net_label_direction := et_schematic.type_net_label_direction'first;

							when others => invalid_section;
						end case;
						
					when SEC_LINE =>
						case stack.parent is
							when SEC_CONTOURS => add_polygon_line (board_line);
								
							when SEC_ROUTE =>

								-- insert line in route.lines
								et_pcb.pac_copper_lines.append (
									container	=> route.lines,
									new_item	=> (et_packages.pac_shapes.type_line (board_line) with
											width	=> board_line_width,
											layer	=> signal_layer));
									
								board_reset_line;
								board_reset_line_width;
								board_reset_signal_layer;

							when SEC_TOP =>
								case stack.parent (degree => 2) is
									when SEC_SILK_SCREEN =>
										insert_line (
											layer	=> SILK_SCREEN,
											face	=> et_pcb_coordinates.TOP);

									when SEC_ASSEMBLY_DOCUMENTATION =>
										insert_line (
											layer	=> ASSEMBLY_DOCUMENTATION,
											face	=> et_pcb_coordinates.TOP);

									when SEC_STENCIL =>
										insert_line (
											layer	=> STENCIL,
											face	=> et_pcb_coordinates.TOP);

									when SEC_STOP_MASK =>
										insert_line (
											layer	=> STOP_MASK,
											face	=> et_pcb_coordinates.TOP);

									when SEC_KEEPOUT =>
										insert_line (
											layer	=> KEEPOUT,
											face	=> et_pcb_coordinates.TOP);
										
									when others => invalid_section;
								end case;

							when SEC_BOTTOM =>
								case stack.parent (degree => 2) is
									when SEC_SILK_SCREEN =>
										insert_line (
											layer	=> SILK_SCREEN,
											face	=> et_pcb_coordinates.BOTTOM);

									when SEC_ASSEMBLY_DOCUMENTATION =>
										insert_line (
											layer	=> ASSEMBLY_DOCUMENTATION,
											face	=> et_pcb_coordinates.BOTTOM);

									when SEC_STENCIL =>
										insert_line (
											layer	=> STENCIL,
											face	=> et_pcb_coordinates.BOTTOM);

									when SEC_STOP_MASK =>
										insert_line (
											layer	=> STOP_MASK,
											face	=> et_pcb_coordinates.BOTTOM);

									when SEC_KEEPOUT =>
										insert_line (
											layer	=> KEEPOUT,
											face	=> et_pcb_coordinates.BOTTOM);
										
									when others => invalid_section;
								end case;

							when SEC_ROUTE_RESTRICT =>
								insert_line_route_restrict;

							when SEC_VIA_RESTRICT =>
								insert_line_via_restrict;

							when SEC_COPPER =>
								insert_line_track;

							when SEC_PCB_CONTOURS_NON_PLATED =>
								insert_line_contour;
								
							when others => invalid_section;
						end case;
						
					when SEC_ARC =>
						case stack.parent is
							when SEC_CONTOURS => add_polygon_arc (board_arc);

							when SEC_ROUTE =>

								-- insert arc in route.arcs
								et_pcb.pac_copper_arcs.append (
									container	=> route.arcs,
									new_item	=> (et_packages.pac_shapes.type_arc (board_arc) with
											width	=> board_line_width,
											layer	=> signal_layer));
									
								board_reset_arc;
								board_reset_line_width;
								board_reset_signal_layer;
								
							when SEC_TOP =>
								case stack.parent (degree => 2) is
									when SEC_SILK_SCREEN =>
										insert_arc (
											layer	=> SILK_SCREEN,
											face	=> et_pcb_coordinates.TOP);

									when SEC_ASSEMBLY_DOCUMENTATION =>
										insert_arc (
											layer	=> ASSEMBLY_DOCUMENTATION,
											face	=> et_pcb_coordinates.TOP);

									when SEC_STENCIL =>
										insert_arc (
											layer	=> STENCIL,
											face	=> et_pcb_coordinates.TOP);

									when SEC_STOP_MASK =>
										insert_arc (
											layer	=> STOP_MASK,
											face	=> et_pcb_coordinates.TOP);

									when SEC_KEEPOUT =>
										insert_arc (
											layer	=> KEEPOUT,
											face	=> et_pcb_coordinates.TOP);
										
									when others => invalid_section;
								end case;

							when SEC_BOTTOM =>
								case stack.parent (degree => 2) is
									when SEC_SILK_SCREEN =>
										insert_arc (
											layer	=> SILK_SCREEN,
											face	=> et_pcb_coordinates.BOTTOM);

									when SEC_ASSEMBLY_DOCUMENTATION =>
										insert_arc (
											layer	=> ASSEMBLY_DOCUMENTATION,
											face	=> et_pcb_coordinates.BOTTOM);

									when SEC_STENCIL =>
										insert_arc (
											layer	=> STENCIL,
											face	=> et_pcb_coordinates.BOTTOM);

									when SEC_STOP_MASK =>
										insert_arc (
											layer	=> STOP_MASK,
											face	=> et_pcb_coordinates.BOTTOM);

									when SEC_KEEPOUT =>
										insert_arc (
											layer	=> KEEPOUT,
											face	=> et_pcb_coordinates.BOTTOM);
										
									when others => invalid_section;
								end case;

							when SEC_ROUTE_RESTRICT =>
								insert_arc_route_restrict;

							when SEC_VIA_RESTRICT =>
								insert_arc_via_restrict;

							when SEC_COPPER =>
								insert_arc_track;

							when SEC_PCB_CONTOURS_NON_PLATED =>
								insert_arc_contour;
								
							when others => invalid_section;
						end case;

					when SEC_CIRCLE =>
						case stack.parent is
							when SEC_CONTOURS => add_polygon_circle (board_circle);
							
							when SEC_TOP =>
								case stack.parent (degree => 2) is
									when SEC_SILK_SCREEN =>
										insert_circle (
											layer	=> SILK_SCREEN,
											face	=> et_pcb_coordinates.TOP);

									when SEC_ASSEMBLY_DOCUMENTATION =>
										insert_circle (
											layer	=> ASSEMBLY_DOCUMENTATION,
											face	=> et_pcb_coordinates.TOP);

									when SEC_STENCIL =>
										insert_circle (
											layer	=> STENCIL,
											face	=> et_pcb_coordinates.TOP);

									when SEC_STOP_MASK =>
										insert_circle (
											layer	=> STOP_MASK,
											face	=> et_pcb_coordinates.TOP);

									when SEC_KEEPOUT =>
										insert_circle (
											layer	=> KEEPOUT,
											face	=> et_pcb_coordinates.TOP);
										
									when others => invalid_section;
								end case;

							when SEC_BOTTOM =>
								case stack.parent (degree => 2) is
									when SEC_SILK_SCREEN =>
										insert_circle (
											layer	=> SILK_SCREEN,
											face	=> et_pcb_coordinates.BOTTOM);

									when SEC_ASSEMBLY_DOCUMENTATION =>
										insert_circle (
											layer	=> ASSEMBLY_DOCUMENTATION,
											face	=> et_pcb_coordinates.BOTTOM);

									when SEC_STENCIL =>
										insert_circle (
											layer	=> STENCIL,
											face	=> et_pcb_coordinates.BOTTOM);

									when SEC_STOP_MASK =>
										insert_circle (
											layer	=> STOP_MASK,
											face	=> et_pcb_coordinates.BOTTOM);

									when SEC_KEEPOUT =>
										insert_circle (
											layer	=> KEEPOUT,
											face	=> et_pcb_coordinates.BOTTOM);
										
									when others => invalid_section;
								end case;

							when SEC_ROUTE_RESTRICT =>
								insert_circle_route_restrict;

							when SEC_VIA_RESTRICT =>
								insert_circle_via_restrict;

							when SEC_COPPER =>
								insert_circle_track;

							when SEC_PCB_CONTOURS_NON_PLATED =>
								insert_circle_contour;
								
							when others => invalid_section;
						end case;
						
					when SEC_VIA =>
						case stack.parent is
							when SEC_ROUTE =>

								-- insert via in route.vias
								et_pcb.pac_vias.append (route.vias, route_via);
								route_via := (others => <>); -- clean up for next via

							when others => invalid_section;
						end case;

					when SEC_CUTOUT_ZONE =>
						case stack.parent is
							when SEC_ROUTE =>
								build_route_cutout;

							when SEC_TOP =>
								case stack.parent (degree => 2) is
									when SEC_SILK_SCREEN =>
										insert_cutout (
											layer	=> SILK_SCREEN,
											face	=> et_pcb_coordinates.TOP);

									when SEC_ASSEMBLY_DOCUMENTATION =>
										insert_cutout (
											layer	=> ASSEMBLY_DOCUMENTATION,
											face	=> et_pcb_coordinates.TOP);

									when SEC_STENCIL =>
										insert_cutout (
											layer	=> STENCIL,
											face	=> et_pcb_coordinates.TOP);

									when SEC_STOP_MASK =>
										insert_cutout (
											layer	=> STOP_MASK,
											face	=> et_pcb_coordinates.TOP);

									when SEC_KEEPOUT =>
										insert_cutout (
											layer	=> KEEPOUT,
											face	=> et_pcb_coordinates.TOP);
										
									when others => invalid_section;
								end case;

							when SEC_BOTTOM =>
								case stack.parent (degree => 2) is
									when SEC_SILK_SCREEN =>
										insert_cutout (
											layer	=> SILK_SCREEN,
											face	=> et_pcb_coordinates.BOTTOM);

									when SEC_ASSEMBLY_DOCUMENTATION =>
										insert_cutout (
											layer	=> ASSEMBLY_DOCUMENTATION,
											face	=> et_pcb_coordinates.BOTTOM);

									when SEC_STENCIL =>
										insert_cutout (
											layer	=> STENCIL,
											face	=> et_pcb_coordinates.BOTTOM);

									when SEC_STOP_MASK =>
										insert_cutout (
											layer	=> STOP_MASK,
											face	=> et_pcb_coordinates.BOTTOM);

									when SEC_KEEPOUT =>
										insert_cutout (
											layer	=> KEEPOUT,
											face	=> et_pcb_coordinates.BOTTOM);
										
									when others => invalid_section;
								end case;

							when SEC_ROUTE_RESTRICT =>
								insert_cutout_route_restrict;

							when SEC_VIA_RESTRICT =>
								insert_cutout_via_restrict;

							when SEC_COPPER =>
								insert_cutout_copper;
								
							when others => invalid_section;
						end case;
						
					when SEC_FILL_ZONE =>
						case stack.parent is
							when SEC_ROUTE =>
								build_route_polygon;

							when SEC_TOP =>
								case stack.parent (degree => 2) is
									when SEC_SILK_SCREEN =>
										insert_polygon (
											layer	=> SILK_SCREEN,
											face	=> et_pcb_coordinates.TOP);

									when SEC_ASSEMBLY_DOCUMENTATION =>
										insert_polygon (
											layer	=> ASSEMBLY_DOCUMENTATION,
											face	=> et_pcb_coordinates.TOP);

									when SEC_STENCIL =>
										insert_polygon (
											layer	=> STENCIL,
											face	=> et_pcb_coordinates.TOP);

									when SEC_STOP_MASK =>
										insert_polygon (
											layer	=> STOP_MASK,
											face	=> et_pcb_coordinates.TOP);

									when SEC_KEEPOUT =>
										insert_polygon (
											layer	=> KEEPOUT,
											face	=> et_pcb_coordinates.TOP);
										
									when others => invalid_section;
								end case;

							when SEC_BOTTOM =>
								case stack.parent (degree => 2) is
									when SEC_SILK_SCREEN =>
										insert_polygon (
											layer	=> SILK_SCREEN,
											face	=> et_pcb_coordinates.BOTTOM);

									when SEC_ASSEMBLY_DOCUMENTATION =>
										insert_polygon (
											layer	=> ASSEMBLY_DOCUMENTATION,
											face	=> et_pcb_coordinates.BOTTOM);

									when SEC_STENCIL =>
										insert_polygon (
											layer	=> STENCIL,
											face	=> et_pcb_coordinates.BOTTOM);

									when SEC_STOP_MASK =>
										insert_polygon (
											layer	=> STOP_MASK,
											face	=> et_pcb_coordinates.BOTTOM);

									when SEC_KEEPOUT =>
										insert_polygon (
											layer	=> KEEPOUT,
											face	=> et_pcb_coordinates.BOTTOM);
										
									when others => invalid_section;
								end case;

							when SEC_ROUTE_RESTRICT =>
								insert_polygon_route_restrict;

							when SEC_VIA_RESTRICT =>
								insert_polygon_via_restrict;

							when SEC_COPPER =>
								insert_polygon_copper;
								
							when others => invalid_section;
						end case;

					when SEC_SUBMODULE =>
						case stack.parent is
							when SEC_SUBMODULES =>

								-- insert submodule
								update_element (
									container	=> modules,
									position	=> module_cursor,
									process		=> insert_submodule'access);

							when others => invalid_section;
						end case;

					when SEC_PORT =>
						case stack.parent is
							when SEC_PORTS =>
								case stack.parent (degree => 2) is
									when SEC_SUBMODULE =>
										declare
											cursor : submodules.type_submodule_ports.cursor;
											inserted : boolean;
										begin
											-- append port to collection of submodule ports
											submodules.type_submodule_ports.insert (
												container	=> submodule_ports,
												key			=> submodule_port_name, -- RESET
												new_item	=> submodule_port,
												inserted	=> inserted,
												position	=> cursor
												);

											if not inserted then
												log (ERROR, "port " & 
													 et_general.to_string (submodule_port_name) & " already used !",
													 console => true
													);
												raise constraint_error;
											end if;
										end;

										-- clean up for next port
										submodule_port_name := to_net_name ("");
										submodule_port := (others => <>);

									when others => invalid_section;
								end case;

							when others => invalid_section;
						end case;
						
					when SEC_SUBMODULES =>
						case stack.parent is
							when SEC_INIT => null;
							when others => invalid_section;
						end case;

					when SEC_SCHEMATIC =>
						case stack.parent is
							when SEC_DRAWING_FRAMES =>

								-- set schematic frame template
								update_element (
									container	=> modules,
									position	=> module_cursor,
									process		=> set_frame_schematic'access);

							when SEC_DRAWING_GRID => null; -- nothing to do

							when SEC_META =>
								meta_schematic := (meta_basic with others => <>);
								
							when others => invalid_section;
						end case;

					when SEC_BOARD =>
						case stack.parent is
							when SEC_INIT => null;

							when SEC_DRAWING_FRAMES =>
								
								-- set board/layout frame template
								update_element (
									container	=> modules,
									position	=> module_cursor,
									process		=> set_frame_board'access);

							when SEC_DRAWING_GRID => null; -- nothing to do

							when SEC_META =>
								meta_board := (meta_basic with others => <>);
							
							when others => invalid_section;
						end case;

					when SEC_TEXT =>
						case stack.parent is
							when SEC_TEXTS =>

								-- insert note
								update_element (
									container	=> modules,
									position	=> module_cursor,
									process		=> insert_note'access);

							when SEC_TOP =>
								case stack.parent (degree => 2) is
									when SEC_SILK_SCREEN =>
										insert_text (
											layer	=> SILK_SCREEN,
											face	=> et_pcb_coordinates.TOP);

									when SEC_ASSEMBLY_DOCUMENTATION =>
										insert_text (
											layer	=> ASSEMBLY_DOCUMENTATION,
											face	=> et_pcb_coordinates.TOP);

									when SEC_STOP_MASK =>
										insert_text (
											layer	=> STOP_MASK,
											face	=> et_pcb_coordinates.TOP);

									-- CS
									--when SEC_KEEPOUT =>
									--	insert_text (
									--		layer	=> KEEPOUT,
									--		face	=> et_pcb_coordinates.TOP);
										
									when others => invalid_section;
								end case;

							when SEC_BOTTOM =>
								case stack.parent (degree => 2) is
									when SEC_SILK_SCREEN =>
										insert_text (
											layer	=> SILK_SCREEN,
											face	=> et_pcb_coordinates.BOTTOM);

									when SEC_ASSEMBLY_DOCUMENTATION =>
										insert_text (
											layer	=> ASSEMBLY_DOCUMENTATION,
											face	=> et_pcb_coordinates.BOTTOM);

									when SEC_STOP_MASK =>
										insert_text (
											layer	=> STOP_MASK,
											face	=> et_pcb_coordinates.BOTTOM);

									-- CS
									--when SEC_KEEPOUT =>
									--	insert_text (
									--		layer	=> KEEPOUT,
									--		face	=> et_pcb_coordinates.BOTTOM);
										
									when others => invalid_section;
								end case;

							when SEC_COPPER =>
								insert_board_text;
								
							when others => invalid_section;
						end case;

					when SEC_TEXTS =>
						case stack.parent is
							when SEC_INIT => null;
							when others => invalid_section;
						end case;

					when SEC_DRAWING_FRAMES =>
						case stack.parent is
							when SEC_INIT => null;
							when others => invalid_section;
						end case;
						
					when SEC_PLACEHOLDER =>
						case stack.parent is
							when SEC_PLACEHOLDERS =>
								case stack.parent (degree => 2) is
									when SEC_PACKAGE =>

										-- insert package placeholder in collection of text placeholders
										insert_package_placeholder;

									when SEC_UNIT =>

										-- build temporarily unit placeholder
										build_unit_placeholder;

									when others => invalid_section;
								end case;

							when SEC_TOP =>
								case stack.parent (degree => 2) is
									when SEC_SILK_SCREEN =>
										insert_placeholder (
											layer	=> SILK_SCREEN,
											face	=> et_pcb_coordinates.TOP);

									when SEC_ASSEMBLY_DOCUMENTATION =>
										insert_placeholder (
											layer	=> ASSEMBLY_DOCUMENTATION,
											face	=> et_pcb_coordinates.TOP);

									when SEC_STOP_MASK =>
										insert_placeholder (
											layer	=> STOP_MASK,
											face	=> et_pcb_coordinates.TOP);

									when others => invalid_section;
								end case;
								
							when SEC_BOTTOM =>
								case stack.parent (degree => 2) is
									when SEC_SILK_SCREEN =>
										insert_placeholder (
											layer	=> SILK_SCREEN,
											face	=> et_pcb_coordinates.BOTTOM);

									when SEC_ASSEMBLY_DOCUMENTATION =>
										insert_placeholder (
											layer	=> ASSEMBLY_DOCUMENTATION,
											face	=> et_pcb_coordinates.BOTTOM);

									when SEC_STOP_MASK =>
										insert_placeholder (
											layer	=> STOP_MASK,
											face	=> et_pcb_coordinates.BOTTOM);

									when others => invalid_section;
								end case;

							when SEC_COPPER =>
								insert_board_text_placeholder;
								
							when others => invalid_section;
						end case;

					when SEC_PLACEHOLDERS =>
						case stack.parent is
							when SEC_PACKAGE =>

								-- Insert placeholder collection in temporarily device:
								-- CS: constraint error will arise here if the device is virtual.
								-- issue warning and skip this statement in this case:
								device.text_placeholders :=	device_text_placeholders;

								-- clean up for next collection of placeholders
								device_text_placeholders := (others => <>);

							when SEC_UNIT => null;
								
							when others => invalid_section;
						end case;

					when SEC_PACKAGE =>
						case stack.parent is
							when SEC_DEVICE =>

								-- Assign coordinates of package to temporarily device:
								-- CS: constraint error will arise here if the device is virtual.
								-- issue warning and skip this statement in this case:
								device.position := device_position;

								-- Assign flipped flag
								device.flipped := device_flipped;

								-- reset device package position for next device
								device_position := et_pcb_coordinates.package_position_default;

								-- reset flip flag for next device
								device_flipped := et_pcb.flipped_default;

							when others => invalid_section;
						end case;

					when SEC_UNIT =>
						case stack.parent is
							when SEC_UNITS =>

								-- insert unit in temporarily collection of units
								insert_unit;
													
							when others => invalid_section;
						end case;

					when SEC_UNITS =>
						case stack.parent is
							when SEC_DEVICE =>

								-- insert temporarily collection of units in device
								device.units := device_units;

								-- clear temporarily collection of units for next device
								et_schematic.type_units.clear (device_units);
								
							when others => invalid_section;
						end case;

					when SEC_DEVICE =>
						case stack.parent is
							when SEC_DEVICES =>

								-- insert device (where pointer "device" is pointing at) in the module
								update_element (
									container	=> modules,
									position	=> module_cursor,
									process		=> insert_device'access);
								
							when others => invalid_section;
						end case;

					when SEC_DEVICES =>
						case stack.parent is
							when SEC_INIT => null;
							when others => invalid_section;
						end case;

					when SEC_ASSEMBLY_VARIANT =>
						case stack.parent is
							when SEC_ASSEMBLY_VARIANTS => 

								-- insert the assembly variant in the module
								update_element (
									container	=> modules,
									position	=> module_cursor,
									process		=> insert_assembly_variant'access);
								
							when others => invalid_section;
						end case;

					when SEC_ASSEMBLY_VARIANTS =>
						case stack.parent is
							when SEC_INIT => null; -- CS test if active variant exists
							when others => invalid_section;
						end case;

					when SEC_META =>
						case stack.parent is
							when SEC_INIT => set_meta;
							when others => invalid_section;
						end case;
						
					when SEC_NETCHANGERS =>
						case stack.parent is
							when SEC_INIT => null;
							when others => invalid_section;
						end case;

					when SEC_NETCHANGER =>
						case stack.parent is
							when SEC_NETCHANGERS =>

								-- insert netchanger in module
								update_element (
									container	=> modules,
									position	=> module_cursor,
									process		=> insert_netchanger'access);
								
							when others => invalid_section;
						end case;
						
					when SEC_SILK_SCREEN | SEC_ASSEMBLY_DOCUMENTATION | SEC_STENCIL |
						SEC_STOP_MASK | SEC_KEEPOUT | SEC_ROUTE_RESTRICT | SEC_VIA_RESTRICT |
						SEC_COPPER | SEC_PCB_CONTOURS_NON_PLATED =>
						case stack.parent is
							when SEC_BOARD => null;
							when others => invalid_section;
						end case;

					when SEC_TOP | SEC_BOTTOM =>
						case stack.parent is
							when SEC_SILK_SCREEN | SEC_ASSEMBLY_DOCUMENTATION | SEC_STENCIL |
								SEC_STOP_MASK | SEC_KEEPOUT => null;

							when others => invalid_section;
						end case;
						
					when SEC_INIT => null; -- CS: should never happen
				end case;

-- 				exception when event:
-- 					others => 
-- 						log (text => ada.exceptions.exception_message (event), console => true);
-- 						raise;
				
			end execute_section;
			
			function set (
			-- Tests if the current line is a section header or footer. Returns true in both cases.
			-- Returns false if the current line is neither a section header or footer.
			-- If it is a header, the section name is pushed onto the sections stack.
			-- If it is a footer, the latest section name is popped from the stack.
				section_keyword	: in string; -- [NETS
				section			: in type_section) -- SEC_NETS
				return boolean is 
			begin -- set
				if f (line, 1) = section_keyword then -- section name detected in field 1
					if f (line, 2) = section_begin then -- section header detected in field 2
						stack.push (section);
						log (text => write_enter_section & to_string (section), level => log_threshold + 5);
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
							log (text => write_top_level_reached, level => log_threshold + 5);
						else
							log (text => write_return_to_section & to_string (stack.current), level => log_threshold + 5);
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
			if set (section_net_classes, SEC_NET_CLASSES) then null;
			elsif set (section_net_class, SEC_NET_CLASS) then null;
			elsif set (section_board_layer_stack, SEC_BOARD_LAYER_STACK) then null;			
			elsif set (section_drawing_grid, SEC_DRAWING_GRID) then null;
			elsif set (section_nets, SEC_NETS) then null;
			elsif set (section_net, SEC_NET) then null;
			elsif set (section_strands, SEC_STRANDS) then null;
			elsif set (section_strand, SEC_STRAND) then null;
			elsif set (section_segments, SEC_SEGMENTS) then null;
			elsif set (section_segment, SEC_SEGMENT) then null;
			elsif set (section_labels, SEC_LABELS) then null;
			elsif set (section_label, SEC_LABEL) then null;
			elsif set (section_ports, SEC_PORTS) then null;
			elsif set (section_port, SEC_PORT) then null;				
			elsif set (section_route, SEC_ROUTE) then null;								
			elsif set (section_line, SEC_LINE) then null;								
			elsif set (section_arc, SEC_ARC) then null;
			elsif set (section_cutout_zone, SEC_CUTOUT_ZONE) then null;
			elsif set (section_fill_zone, SEC_FILL_ZONE) then null;								
			elsif set (section_contours, SEC_CONTOURS) then null;								
			elsif set (section_via, SEC_VIA) then null;								
			elsif set (section_submodules, SEC_SUBMODULES) then null;
			elsif set (section_submodule, SEC_SUBMODULE) then null;
			elsif set (section_drawing_frames, SEC_DRAWING_FRAMES) then null;
			elsif set (section_schematic, SEC_SCHEMATIC) then null;
			elsif set (section_board, SEC_BOARD) then null;
			elsif set (section_devices, SEC_DEVICES) then null;
			elsif set (section_device, SEC_DEVICE) then null;
			elsif set (section_units, SEC_UNITS) then null;
			elsif set (section_unit, SEC_UNIT) then null;
			elsif set (section_assembly_variants, SEC_ASSEMBLY_VARIANTS) then null;
			elsif set (section_assembly_variant, SEC_ASSEMBLY_VARIANT) then null;
			elsif set (section_netchangers, SEC_NETCHANGERS) then null;
			elsif set (section_netchanger, SEC_NETCHANGER) then null;
			elsif set (section_meta, SEC_META) then null;
			elsif set (section_placeholders, SEC_PLACEHOLDERS) then null;				
			elsif set (section_placeholder, SEC_PLACEHOLDER) then null;
			elsif set (section_package, SEC_PACKAGE) then null;
			elsif set (section_texts, SEC_TEXTS) then null;
			elsif set (section_text, SEC_TEXT) then null;
			elsif set (section_silk_screen, SEC_SILK_SCREEN) then null;
			elsif set (section_top, SEC_TOP) then null;
			elsif set (section_bottom, SEC_BOTTOM) then null;
			elsif set (section_circle, SEC_CIRCLE) then null;
			elsif set (section_assembly_doc, SEC_ASSEMBLY_DOCUMENTATION) then null;
			elsif set (section_stencil, SEC_STENCIL) then null;
			elsif set (section_stop_mask, SEC_STOP_MASK) then null;
			elsif set (section_keepout, SEC_KEEPOUT) then null;
			elsif set (section_route_restrict, SEC_ROUTE_RESTRICT) then null;
			elsif set (section_via_restrict, SEC_VIA_RESTRICT) then null;
			elsif set (section_copper, SEC_COPPER) then null;				
			elsif set (section_pcb_contours, SEC_PCB_CONTOURS_NON_PLATED) then null;
			else
				-- The line contains something else -> the payload data. 
				-- Temporarily this data is stored in corresponding variables.

				log (text => "module line --> " & to_string (line), level => log_threshold + 4);
		
				case stack.current is

					when SEC_CONTOURS =>
						case stack.parent is
							when SEC_FILL_ZONE => null;
							when SEC_CUTOUT_ZONE => null;
							when others => invalid_section;
						end case;
					
					when SEC_NET_CLASSES =>
						case stack.parent is
							when SEC_INIT => null; -- nothing to do
							when others => invalid_section;
						end case;

					when SEC_DRAWING_GRID =>
						case stack.parent is
							when SEC_INIT => null; -- nothing to do
							when others => invalid_section;
						end case;

					when SEC_BOARD_LAYER_STACK =>
						case stack.parent is
							when SEC_INIT => read_layer;
							when others => invalid_section;
						end case;
						
					when SEC_DEVICES =>
						case stack.parent is
							when SEC_INIT => null; -- nothing to do
							when others => invalid_section;
						end case;

					when SEC_ASSEMBLY_VARIANTS =>
						case stack.parent is
							when SEC_INIT => set_active_assembly_variant;
							when others => invalid_section;
						end case;

					when SEC_ASSEMBLY_VARIANT =>
						case stack.parent is
							when SEC_ASSEMBLY_VARIANTS =>
								declare
									use et_devices;
									kw : string 	:= f (line, 1);
									device_name		: type_name; -- R1
									device			: access assembly_variants.type_device;
									device_cursor	: assembly_variants.type_devices.cursor;
									
									submod_name		: et_general.type_module_instance_name.bounded_string; -- MOT_DRV_3
									submod_var		: et_general.type_variant_name.bounded_string; -- low_cost
									submod_cursor	: assembly_variants.type_submodules.cursor;
									inserted		: boolean;
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_name then -- name low_cost
										expect_field_count (line, 2);
										assembly_variant_name := et_general.to_variant (f (line, 2));

									elsif kw = keyword_description then -- description "variant without temperature sensor"
										expect_field_count (line, 2);

										assembly_variant_description := assembly_variants.to_unbounded_string (f (line, 2));
										
									-- A line like "device R1 not_mounted" or
									-- a line like "device R1 value 270R partcode 12345" or		
									-- a line like "device R1 value 270R partcode 12345 purpose "set temperature""
									-- tells whether a device is mounted or not.
									elsif kw = keyword_device then

										-- there must be at least 3 fields:
										expect_field_count (line, 3, warn => false);
										
										device_name := to_name (f (line, 2));

										-- test whether device exists
										if not exists (module_cursor, device_name) then
											log (ERROR, "device " &
												 enclose_in_quotes (to_string (device_name)) &
												 " does not exist !", console => true);
											raise constraint_error;
										end if;

										if f (line, 3) = keyword_not_mounted then
											-- line like "device R1 not_mounted"

											device := new assembly_variants.type_device'(
												mounted	=> assembly_variants.NO);
											
										elsif f (line, 3) = keyword_value then
											-- line like "device R1 value 270R partcode 12345"

											-- create a device with discriminant "mounted" where
											-- pointer assembly_variant_device is pointing at.
											device := new assembly_variants.type_device'(
												mounted	=> assembly_variants.YES,
												others	=> <>); -- to be assigned later
											
											-- there must be at least 6 fields:
											expect_field_count (line, 6, warn => false);

											-- read and validate value
											device.value := to_value (f (line, 4));

											-- read partcode
											if f (line, 5) = material.keyword_partcode then
												device.partcode := material.to_partcode (f (line, 6));
											else -- keyword partcode not found
												log (ERROR, "expect keyword " & enclose_in_quotes (material.keyword_partcode) &
													 " after value !", console => true);
												raise constraint_error;
											end if;

											-- read optional purpose
											if field_count (line) > 6 then
												expect_field_count (line, 8);

												if f (line, 7) = keyword_purpose then

													-- validate purpose
													device.purpose := to_purpose (f (line, 8));

												else -- keyword purpose not found
													log (ERROR, "expect keyword " & enclose_in_quotes (keyword_purpose) &
														" after partcode !", console => true);
													raise constraint_error;
												end if;
											end if;
												
										else -- keyword value not found
											log (ERROR, "expect keyword " & enclose_in_quotes (keyword_value) &
												 " or keyword " & enclose_in_quotes (keyword_not_mounted) &
												 " after device name !", console => true);
											raise constraint_error;
										end if;											

										-- Insert the device in the current assembly variant:
										assembly_variants.type_devices.insert (
											container	=> assembly_variant_devices,
											key			=> device_name, -- R1
											new_item	=> device.all,
											inserted	=> inserted,
											position	=> device_cursor);

										-- Raise error if device occurs more than once:
										if not inserted then
											log (ERROR, "device " &
												 enclose_in_quotes (to_string (device_name)) &
												 " already specified !", console => true);
											raise constraint_error;
										end if;

									-- a line like "submodule OSC1 variant low_cost
									-- tells which assembly variant of a submodule is used:
									elsif kw = keyword_submodule then

										-- there must be 4 fields:
										expect_field_count (line, 4);

										submod_name := et_general.to_instance_name (f (line, 2)); -- OSC1

										-- test whether submodule instance exists
										if not exists (module_cursor, submod_name) then
											log (ERROR, "submodule instance " &
												 enclose_in_quotes (et_general.to_string (submod_name)) &
												 " does not exist !", console => true);
											raise constraint_error;
										end if;

										-- After the instance name (like OSC1) must come the keyword "variant"
										-- followed by the variant name:
										if f (line, 3) = keyword_variant then
											submod_var := et_general.to_variant (f (line, 4));
											
											-- NOTE: A test whether the submodule does provide the variant can
											-- not be executed at this stage because the submodules have not 
											-- been read yet. This will be done after procdure 
											-- read_submodule_files has been executed. See far below.

											-- Insert the submodule in the current assembly variant:
											assembly_variants.type_submodules.insert (
												container	=> assembly_variant_submodules,
												key			=> submod_name, -- OSC1
												new_item	=> (variant => submod_var), -- type_submodule is a record with currently only one element
												inserted	=> inserted,
												position	=> submod_cursor);

											-- Raise error if submodule occurs more than once:
											if not inserted then
												log (ERROR, "submodule " &
													enclose_in_quotes (et_general.to_string (submod_name)) &
													" already specified !", console => true);
												raise constraint_error;
											end if;

										else
											log (ERROR, "expect keyword " & enclose_in_quotes (keyword_variant) &
												 " after instance name !", console => true);
											raise constraint_error;
										end if;
										
									else
										invalid_keyword (kw);
									end if;
								end;
								
							when others => invalid_section;
						end case;
						
					when SEC_TEXTS =>
						case stack.parent is
							when SEC_INIT => null; -- nothing to do								
							when others => invalid_section;
						end case;

					when SEC_SUBMODULES =>
						case stack.parent is
							when SEC_INIT => null; -- nothing to do								
							when others => invalid_section;
						end case;

					when SEC_DRAWING_FRAMES =>
						case stack.parent is
							when SEC_INIT => null; -- nothing to do								
							when others => invalid_section;
						end case;

					when SEC_META =>
						case stack.parent is
							when SEC_INIT => null; -- nothing to do
							when others => invalid_section;
						end case;
						
					when SEC_NET_CLASS =>
						case stack.parent is
							when SEC_NET_CLASSES =>
								declare
									use et_packages;
									use et_pcb;
									kw : string := f (line, 1);
								begin
									if kw = keyword_name then
										expect_field_count (line, 2);
										net_class_name := to_net_class_name (f (line,2));

									-- CS: In the following: set a corresponding parameter-found-flag
									elsif kw = keyword_description then
										expect_field_count (line, 2);
										net_class.description := to_net_class_description (f (line,2));
										
									elsif kw = keyword_clearance then
										expect_field_count (line, 2);
										net_class.clearance := et_pcb_coordinates.geometry.to_distance (f (line,2));
										validate_track_clearance (net_class.clearance);
										
									elsif kw = keyword_track_width_min then
										expect_field_count (line, 2);
										net_class.track_width_min := et_pcb_coordinates.geometry.to_distance (f (line,2));
										validate_track_width (net_class.track_width_min);
										
									elsif kw = keyword_via_drill_min then
										expect_field_count (line, 2);
										net_class.via_drill_min := et_pcb_coordinates.geometry.to_distance (f (line,2));
										validate_drill_size (net_class.via_drill_min);
										
									elsif kw = keyword_via_restring_min then
										expect_field_count (line, 2);
										net_class.via_restring_min := et_pcb_coordinates.geometry.to_distance (f (line,2));
										validate_restring_width (net_class.via_restring_min);
										
									elsif kw = keyword_micro_via_drill_min then
										expect_field_count (line, 2);
										net_class.micro_via_drill_min := et_pcb_coordinates.geometry.to_distance (f (line,2));
										validate_drill_size (net_class.micro_via_drill_min);
										
									elsif kw = keyword_micro_via_restring_min then
										expect_field_count (line, 2);
										net_class.micro_via_restring_min := et_pcb_coordinates.geometry.to_distance (f (line,2));
										validate_restring_width (net_class.micro_via_restring_min);
									else
										invalid_keyword (kw);
									end if;
								end;

							when others => invalid_section;
						end case;

					when SEC_STRAND =>
						case stack.parent is
							when SEC_STRANDS =>
								declare
									kw : string := f (line, 1);
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_position then -- position sheet 1 x 1.000 y 5.555
										expect_field_count (line, 7);

										-- extract strand position starting at field 2
										strand.position := to_position (line, 2);
									else
										invalid_keyword (kw);
									end if;
								end;
							
							when others => invalid_section;
						end case;
						
					when SEC_STRANDS =>
						case stack.parent is
							when SEC_NET => null; -- nothing to do
							when others => invalid_section;
						end case;
					
					when SEC_ROUTE =>
						case stack.parent is
							when SEC_NET => null; -- nothing to do
							when others => invalid_section;
						end case;
					
					when SEC_NET =>
						case stack.parent is
							when SEC_NETS =>
								declare
									kw : string := f (line, 1);
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_name then
										expect_field_count (line, 2);
										net_name := to_net_name (f (line,2));
										
									elsif kw = keyword_class then
										-- CS: imported kicad projects lack the class name sometimes.
										-- For this reason we do not abort in such cases but issue a warning.
										-- If abort is a must, the next two statements are required. 
										-- The "if" construct must be in comments instead.
										-- It is perhaps more reasonable to care for this flaw in et_kicad_pcb package.
										
										-- expect_field_count (line, 2);
										-- net.class := et_pcb.to_net_class_name (f (line,2));
										
										if field_count (line) = 2 then
											net.class := et_pcb.to_net_class_name (f (line,2));
										else
											net.class := et_pcb.net_class_name_default;
											log (text => message_warning & affected_line (line) & "No net class specified ! Assume default class !");
										end if;
									elsif kw = keyword_scope then
										expect_field_count (line, 2);
										net.scope := netlists.to_net_scope (f (line,2));
										
									else
										invalid_keyword (kw);
									end if;
								end;
										
							when others => invalid_section;
						end case;

					when SEC_NETS =>
						case stack.parent is
							when SEC_INIT => null;
							when others => invalid_section;
						end case;
						
					when SEC_SEGMENT =>
						case stack.parent is
							when SEC_SEGMENTS =>
								declare
									kw : string := f (line, 1);
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_start then -- "start x 3 y 4"
										expect_field_count (line, 5);

										-- extract start position starting at field 2
										net_segment.start_point := to_position (line, from => 2);
										
									elsif kw = keyword_end then -- "end x 6 y 4"
										expect_field_count (line, 5);

										-- extract end position starting at field 2
										net_segment.end_point := to_position (line, from => 2);

									elsif kw = keyword_junction then -- "junction start/end"
										expect_field_count (line, 2);
										set_junction (f (line, 2));
									else
										invalid_keyword (kw);
									end if;
								end;
										
							when others => invalid_section;
						end case;

					when SEC_SEGMENTS =>
						case stack.parent is
							when SEC_STRAND => null; -- nothing to do
							when others => invalid_section;
						end case;
						
					when SEC_LABELS =>
						case stack.parent is
							when SEC_SEGMENT => null; -- nothing to do
							when others => invalid_section;
						end case;
						
					when SEC_PORTS =>
						case stack.parent is 
							when SEC_SEGMENT =>
								-- read port parameters
								-- NOTE: A device, submodule or netchanger port is defined by a
								-- single line.
								-- Upon reading the line like "device/submodule/netchanger x port 1" 
								-- the port is appended to the corresponding port collection 
								-- immediately when the line is read. See main code of process_line.
								declare
									use et_symbols;
									kw : string := f (line, 1);
								begin
									if kw = keyword_device then -- device R1 port 1
										expect_field_count (line, 4);

										net_device_port.device_name := to_name (f (line, 2)); -- IC3

										if f (line, 3) = keyword_port then -- port
											net_device_port.port_name := to_port_name (f (line, 4)); -- CE

											-- Insert port in port collection of device ports. First make sure it is
											-- not already in the net segment.
											if et_schematic.type_ports_device.contains (net_device_ports, net_device_port) then
												log (ERROR, "device " & to_string (net_device_port.device_name) &
													" port " & to_string (net_device_port.port_name) & 
													" already in net segment !", console => true);
												raise constraint_error;
											end if;

											et_schematic.type_ports_device.insert (net_device_ports, net_device_port); 

										else
											invalid_keyword (f (line, 3));
										end if;

									elsif kw = keyword_submodule then -- submodule motor_driver port mot_on_off
										expect_field_count (line, 4);
										
										net_submodule_port.module_name := et_general.to_instance_name (f (line, 2)); -- motor_driver

										if f (line, 3) = keyword_port then -- port
											net_submodule_port.port_name := to_net_name (f (line, 4)); -- A

											-- Insert submodule port in collection of submodule ports. First make sure it is
											-- not already in the net segment.
											if et_schematic.type_ports_submodule.contains (net_submodule_ports, net_submodule_port) then
												log (ERROR, "submodule " & to_string (net_submodule_port.module_name) &
													" port " & et_general.to_string (net_submodule_port.port_name) & 
													" already in net segment !", console => true);
												raise constraint_error;
											end if;
											
											et_schematic.type_ports_submodule.insert (net_submodule_ports, net_submodule_port);

											-- clean up for next submodule port
											net_submodule_port := (others => <>);

										else
											invalid_keyword (f (line, 3));
										end if;

									elsif kw = keyword_netchanger then -- netchanger 1 port master/slave
										expect_field_count (line, 4);
										
										net_netchanger_port.index := submodules.to_netchanger_id (f (line, 2)); -- 1

										if f (line, 3) = keyword_port then -- port
											net_netchanger_port.port := submodules.to_port_name (f (line, 4)); -- MASTER, SLAVE

											-- Insert netchanger port in collection of netchanger ports. First make sure it is
											-- not already in the net segment.
											if netlists.type_ports_netchanger.contains (net_netchanger_ports, net_netchanger_port) then
												log (ERROR, "netchanger" & submodules.to_string (net_netchanger_port.index) &
													submodules.to_string (net_netchanger_port.port) & " port" & 
													" already in net segment !", console => true);
												raise constraint_error;
											end if;
											
											netlists.type_ports_netchanger.insert (net_netchanger_ports, net_netchanger_port);

											-- clean up for next netchanger port
											net_netchanger_port := (others => <>);

										else
											invalid_keyword (f (line, 3));
										end if;
										
									else
										invalid_keyword (kw);
									end if;
								end;

							when SEC_SUBMODULE => null; -- nothing to do
								
							when others => invalid_section;
						end case;

					when SEC_LABEL =>
						case stack.parent is
							when SEC_LABELS =>
								declare
									kw : string := f (line, 1);
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_position then -- position x 148.59 y 104.59
										expect_field_count (line, 5);

										-- extract label position starting at field 2 of line
										net_label.position := to_position (line, 2);
										
									elsif kw = keyword_rotation then -- rotation 0.0
										expect_field_count (line, 2);
										net_label.rotation := geometry.to_rotation (f (line, 2));

									elsif kw = et_text.keyword_size then -- size 1.3
										expect_field_count (line, 2);
										net_label.size := geometry.to_distance (f (line, 2));

									elsif kw = keyword_style then -- style normal
										expect_field_count (line, 2);
										net_label.style := et_symbols.to_text_style (f (line, 2));

									elsif kw = et_text.keyword_line_width then -- line_width 0.1
										expect_field_count (line, 2);
										net_label.width := et_coordinates.geometry.to_distance (f (line, 2));

									elsif kw = keyword_appearance then -- appearance tag/simple
										expect_field_count (line, 2);
										net_label_appearance := et_schematic.to_appearance (f (line, 2));

									elsif kw = et_schematic.keyword_direction then -- direction input/output
										expect_field_count (line, 2);
										net_label_direction := et_schematic.to_direction (f (line, 2));
										
									else
										invalid_keyword (kw);
									end if;
								end;

							when others => invalid_section;
						end case;
					
					when SEC_LINE =>
						case stack.parent is

							when SEC_CONTOURS => read_board_line (line); -- of a cutout or fill zone
								
							when SEC_ROUTE =>
								if not read_board_line (line) then
									declare
										kw : string := f (line, 1);
										use et_pcb_stack;
									begin
										-- CS: In the following: set a corresponding parameter-found-flag
										if kw = keyword_layer then -- layer 2
											expect_field_count (line, 2);
											signal_layer := et_pcb_stack.to_signal_layer (f (line, 2));

										elsif kw = pcb_rw.keyword_width then -- width 0.5
											expect_field_count (line, 2);
											board_line_width := et_pcb_coordinates.geometry.to_distance (f (line, 2));
											
										else
											invalid_keyword (kw);
										end if;
									end;
								end if;
								
							when SEC_TOP | SEC_BOTTOM => 
								case stack.parent (degree => 2) is
									when SEC_SILK_SCREEN | SEC_ASSEMBLY_DOCUMENTATION |
										SEC_STENCIL | SEC_STOP_MASK =>

										if not read_board_line (line) then
											declare
												kw : string := f (line, 1);
											begin
												-- CS: In the following: set a corresponding parameter-found-flag
												if kw = pcb_rw.keyword_width then -- width 0.5
													expect_field_count (line, 2);
													board_line_width := et_pcb_coordinates.geometry.to_distance (f (line, 2));
													
												else
													invalid_keyword (kw);
												end if;
											end;
										end if;
										
									when SEC_KEEPOUT => read_board_line (line);
										
									when others => invalid_section;
								end case;
								
							when SEC_ROUTE_RESTRICT | SEC_VIA_RESTRICT =>
								
								if not read_board_line (line) then
									declare
										kw : string := f (line, 1);
										use et_pcb_stack;
									begin
										-- CS: In the following: set a corresponding parameter-found-flag
										if kw = keyword_layers then -- layers 1 14 3

											-- there must be at least two fields:
											expect_field_count (line => line, count_expected => 2, warn => false);

											signal_layers := to_layers (line);
										else
											invalid_keyword (kw);
										end if;
									end;
								end if;
								
							when SEC_COPPER =>
								if not read_board_line (line) then
									declare
										kw : string := f (line, 1);
										use et_pcb_stack;
									begin
										-- CS: In the following: set a corresponding parameter-found-flag
										if kw = pcb_rw.keyword_width then -- width 0.5
											expect_field_count (line, 2);
											board_line_width := et_pcb_coordinates.geometry.to_distance (f (line, 2));

										elsif kw = keyword_layer then -- layer 1
											expect_field_count (line, 2);
											signal_layer := et_pcb_stack.to_signal_layer (f (line, 2));
										else
											invalid_keyword (kw);
										end if;
									end;
								end if;
								
							when SEC_PCB_CONTOURS_NON_PLATED => 

								if not read_board_line (line) then
									declare
										kw : string := f (line, 1);
									begin
										-- CS: In the following: set a corresponding parameter-found-flag
										if kw = keyword_locked then -- locked no
											expect_field_count (line, 2);
											board_lock_status := et_pcb.to_lock_status (f (line, 2));
											
										else
											invalid_keyword (kw);
										end if;
									end;
								end if;
								
							when others => invalid_section;
						end case;

					when SEC_ARC =>
						case stack.parent is

							when SEC_CONTOURS => read_board_arc (line);
							
							when SEC_ROUTE =>
								if not read_board_arc (line) then
									declare
										kw : string := f (line, 1);
										use et_packages.pac_shapes;
										use et_pcb_stack;
									begin
										-- CS: In the following: set a corresponding parameter-found-flag
										if kw = keyword_layer then -- layer 2
											expect_field_count (line, 2);
											signal_layer := et_pcb_stack.to_signal_layer (f (line, 2));

										elsif kw = pcb_rw.keyword_width then -- width 0.5
											expect_field_count (line, 2);
											board_line_width := et_pcb_coordinates.geometry.to_distance (f (line, 2));
											
										else
											invalid_keyword (kw);
										end if;
									end;
								end if;
									
							when SEC_TOP | SEC_BOTTOM => 
								case stack.parent (degree => 2) is
									when SEC_SILK_SCREEN | SEC_ASSEMBLY_DOCUMENTATION |
										SEC_STENCIL | SEC_STOP_MASK =>

										if not read_board_arc (line) then
										
											declare
												kw : string := f (line, 1);
											begin
												-- CS: In the following: set a corresponding parameter-found-flag
												if kw = pcb_rw.keyword_width then -- width 0.5
													expect_field_count (line, 2);
													board_line_width := et_pcb_coordinates.geometry.to_distance (f (line, 2));
													
												else
													invalid_keyword (kw);
												end if;
											end;

										end if;
										
									when SEC_KEEPOUT => read_board_arc (line);
										
									when others => invalid_section;
								end case;

							when SEC_ROUTE_RESTRICT | SEC_VIA_RESTRICT =>
								if not read_board_arc (line) then
								
									declare
										kw : string := f (line, 1);
										use et_pcb_stack;
									begin
										-- CS: In the following: set a corresponding parameter-found-flag
										if kw = keyword_layers then -- layers 1 14 3

											-- there must be at least two fields:
											expect_field_count (line => line, count_expected => 2, warn => false);

											signal_layers := to_layers (line);

										else
											invalid_keyword (kw);
										end if;
									end;

								end if;
								
							when SEC_COPPER =>
								if not read_board_arc (line) then
									declare
										kw : string := f (line, 1);
										use et_packages.pac_shapes;
										use et_pcb_stack;
									begin
										-- CS: In the following: set a corresponding parameter-found-flag
										if kw = pcb_rw.keyword_width then -- width 0.5
											expect_field_count (line, 2);
											board_line_width := et_pcb_coordinates.geometry.to_distance (f (line, 2));

										elsif kw = keyword_layer then -- layer 1
											expect_field_count (line, 2);
											signal_layer := et_pcb_stack.to_signal_layer (f (line, 2));
										else
											invalid_keyword (kw);
										end if;
									end;
								end if;
								
							when SEC_PCB_CONTOURS_NON_PLATED =>
								if not read_board_arc (line) then
									declare
										kw : string := f (line, 1);
										use et_packages.pac_shapes;
									begin
										-- CS: In the following: set a corresponding parameter-found-flag
										if kw = keyword_locked then -- locked no
											expect_field_count (line, 2);
											board_lock_status := et_pcb.to_lock_status (f (line, 2));
											
										else
											invalid_keyword (kw);
										end if;
									end;
								end if;
								
							when others => invalid_section;
						end case;

					when SEC_CIRCLE =>
						case stack.parent is

							when SEC_CONTOURS => read_board_circle (line);
							
							when SEC_TOP | SEC_BOTTOM => 
								case stack.parent (degree => 2) is
									when SEC_SILK_SCREEN | SEC_ASSEMBLY_DOCUMENTATION |
										SEC_STENCIL | SEC_STOP_MASK =>

										if not read_board_circle (line) then
										
											declare
												use et_packages;
												use et_packages.pac_shapes;
												kw : string := f (line, 1);
											begin
												-- CS: In the following: set a corresponding parameter-found-flag
												if kw = pcb_rw.keyword_width then -- circumfence line width 0.5
													expect_field_count (line, 2);
													board_line_width := et_pcb_coordinates.geometry.to_distance (f (line, 2));

												elsif kw = keyword_filled then -- filled yes/no
													expect_field_count (line, 2);													
													board_filled := to_filled (f (line, 2));

												elsif kw = keyword_fill_style then -- fill_style solid/hatched
													expect_field_count (line, 2);													
													board_fill_style := to_fill_style (f (line, 2));

												elsif kw = keyword_hatching_line_width then -- hatching_line_width 0.3
													expect_field_count (line, 2);													
													board_hatching.line_width := et_pcb_coordinates.geometry.to_distance (f (line, 2));

												elsif kw = keyword_hatching_line_spacing then -- hatching_line_spacing 0.3
													expect_field_count (line, 2);													
													board_hatching.spacing := et_pcb_coordinates.geometry.to_distance (f (line, 2));
													
												else
													invalid_keyword (kw);
												end if;
											end;

										end if;
										
									when SEC_KEEPOUT => read_board_circle (line);
										
									when others => invalid_section;

								end case;

							when SEC_ROUTE_RESTRICT | SEC_VIA_RESTRICT =>
								if not read_board_circle (line) then

									declare
										use et_pcb_stack;
										use et_packages;
										use et_packages.pac_shapes;
										use et_pcb_coordinates.geometry;
										kw : string := f (line, 1);
									begin
										-- CS: In the following: set a corresponding parameter-found-flag
										if kw = keyword_filled then -- filled yes/no
											expect_field_count (line, 2);													
											board_filled := to_filled (f (line, 2));

										elsif kw = keyword_layers then -- layers 1 14 3

											-- there must be at least two fields:
											expect_field_count (line => line, count_expected => 2, warn => false);

											signal_layers := to_layers (line);
											
										else
											invalid_keyword (kw);
										end if;
									end;

								end if;
								
							when SEC_COPPER =>
								if not read_board_circle (line) then
									declare
										use et_packages;
										use et_packages.pac_shapes;
										use et_pcb_stack;
										use et_pcb_coordinates.geometry;
										kw : string := f (line, 1);
									begin
										-- CS: In the following: set a corresponding parameter-found-flag
										if kw = pcb_rw.keyword_width then -- width 0.5
											expect_field_count (line, 2);
											board_line_width := to_distance (f (line, 2));

										elsif kw = keyword_filled then -- filled yes/no
											expect_field_count (line, 2);													
											board_filled := to_filled (f (line, 2));

										elsif kw = keyword_fill_style then -- fill_style solid/hatched
											expect_field_count (line, 2);													
											board_fill_style := to_fill_style (f (line, 2));

										elsif kw = keyword_hatching_line_width then -- hatching_line_width 0.3
											expect_field_count (line, 2);													
											board_hatching_copper.line_width := to_distance (f (line, 2));

										elsif kw = keyword_hatching_line_spacing then -- hatching_line_spacing 0.3
											expect_field_count (line, 2);													
											board_hatching_copper.spacing := to_distance (f (line, 2));

										elsif kw = keyword_hatching_border_width then -- hatching_border_width 1
											expect_field_count (line, 2);													
											board_hatching_copper.border_width := to_distance (f (line, 2));
											
										elsif kw = keyword_layer then -- layer 1
											expect_field_count (line, 2);
											signal_layer := et_pcb_stack.to_signal_layer (f (line, 2));
										else
											invalid_keyword (kw);
										end if;
									end;
								end if;
								
							when SEC_PCB_CONTOURS_NON_PLATED =>
								if not read_board_circle (line) then
									declare
										use et_pcb_coordinates.geometry;
										use et_packages.pac_shapes;
										kw : string := f (line, 1);
									begin
										-- CS: In the following: set a corresponding parameter-found-flag
										if kw = keyword_locked then -- locked no
											expect_field_count (line, 2);
											board_lock_status := et_pcb.to_lock_status (f (line, 2));
											
										else
											invalid_keyword (kw);
										end if;
									end;
								end if;
								
							when others => invalid_section;
						end case;

					when SEC_CUTOUT_ZONE =>
						case stack.parent is
							when SEC_ROUTE =>
								declare
									use et_packages;
									use et_pcb_coordinates.geometry;
									use et_pcb_stack;
									kw : string := f (line, 1);
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_corner_easing then -- corner_easing none/chamfer/fillet
										expect_field_count (line, 2);
										board_easing.style := to_corner_easing (f (line, 2));

									elsif kw = keyword_easing_radius then -- easing_radius 0.3
										expect_field_count (line, 2);
										board_easing.radius := to_distance (f (line, 2));

									elsif kw = keyword_layer then -- layer 2
										expect_field_count (line, 2);
										signal_layer := et_pcb_stack.to_signal_layer (f (line, 2));

									else
										invalid_keyword (kw);
									end if;
								end;

							when SEC_TOP | SEC_BOTTOM => 
								case stack.parent (degree => 2) is
									when SEC_SILK_SCREEN | SEC_ASSEMBLY_DOCUMENTATION |
										SEC_STENCIL | SEC_STOP_MASK =>
										declare
											use et_packages;
											use et_packages.pac_shapes;
											use et_pcb_coordinates.geometry;
											kw : string := f (line, 1);
										begin
											-- CS: In the following: set a corresponding parameter-found-flag
											if kw = keyword_corner_easing then -- corner_easing none/chamfer/fillet
												expect_field_count (line, 2);													
												board_easing.style := to_corner_easing (f (line, 2));

											elsif kw = keyword_easing_radius then -- easing_radius 0.4
												expect_field_count (line, 2);													
												board_easing.radius := to_distance (f (line, 2));
												
											else
												invalid_keyword (kw);
											end if;
										end;

									when SEC_KEEPOUT =>
										-- no parameters allowed here
										declare
											kw : string := f (line, 1);
										begin
											invalid_keyword (kw);
										end;
										
									when others => invalid_section;
								end case;

							when SEC_ROUTE_RESTRICT | SEC_VIA_RESTRICT =>
								declare
									use et_pcb_stack;
									use et_packages;
									use et_packages.pac_shapes;
									use et_pcb_coordinates.geometry;
									kw : string := f (line, 1);
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_layers then -- layers 1 14 3

										-- there must be at least two fields:
										expect_field_count (line => line, count_expected => 2, warn => false);
										signal_layers := to_layers (line);

									else
										invalid_keyword (kw);
									end if;
								end;

							when SEC_COPPER => -- non electrical
								declare
									use et_packages;
									use et_packages.pac_shapes;									
									use et_pcb_stack;
									use et_pcb_coordinates.geometry;
									kw : string := f (line, 1);
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_corner_easing then -- corner_easing none/chamfer/fillet
										expect_field_count (line, 2);													
										board_easing.style := to_corner_easing (f (line, 2));

									elsif kw = keyword_easing_radius then -- easing_radius 0.4
										expect_field_count (line, 2);													
										board_easing.radius := to_distance (f (line, 2));
										
									elsif kw = keyword_layer then -- layer 1
										expect_field_count (line, 2);
										signal_layer := et_pcb_stack.to_signal_layer (f (line, 2));

									else
										invalid_keyword (kw);
									end if;
								end;
								
							when others => invalid_section;
						end case;
						
					when SEC_FILL_ZONE =>
						case stack.parent is
							when SEC_ROUTE => -- connected with a net
								declare
									use et_packages;
									use et_pcb_coordinates.geometry;
									use et_pcb;
									use et_pcb_stack;
									kw : string := f (line, 1);
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = et_pcb.keyword_priority then -- priority 2
										expect_field_count (line, 2);
										polygon_priority := et_pcb.to_polygon_priority (f (line, 2));

									elsif kw = keyword_isolation then -- isolation 0.5
										expect_field_count (line, 2);
										polygon_isolation := to_distance (f (line, 2));
										
									elsif kw = keyword_corner_easing then -- corner_easing none/chamfer/fillet
										expect_field_count (line, 2);
										board_easing.style := to_corner_easing (f (line, 2));

									elsif kw = keyword_easing_radius then -- easing_radius 0.3
										expect_field_count (line, 2);
										board_easing.radius := to_distance (f (line, 2));

									elsif kw = keyword_fill_style then -- fill_style solid,hatched
										expect_field_count (line, 2);
										board_fill_style := to_fill_style (f (line, 2));

									elsif kw = keyword_hatching_line_width then -- hatching_line_width 1
										expect_field_count (line, 2);
										board_hatching_copper.line_width := to_distance (f (line, 2));

									elsif kw = keyword_hatching_line_spacing then -- hatching_line_spacing 1
										expect_field_count (line, 2);
										board_hatching_copper.spacing := to_distance (f (line, 2));

									elsif kw = keyword_hatching_border_width then -- hatching_border_width 1
										expect_field_count (line, 2);
										board_hatching_copper.border_width := to_distance (f (line, 2));
										
									elsif kw = keyword_layer then -- layer 2
										expect_field_count (line, 2);
										signal_layer := et_pcb_stack.to_signal_layer (f (line, 2));

									elsif kw = keyword_min_width then -- min_width 0.3
										expect_field_count (line, 2);
										polygon_width_min := to_distance (f (line, 2));

									elsif kw = keyword_pad_technology then -- pad_technology smt_only/tht_only/smt_and_tht
										expect_field_count (line, 2);
										pcb_rw.thermal.technology := to_pad_technology (f (line, 2));

									elsif kw = keyword_pad_connection then -- pad_connection thermal/solid
										expect_field_count (line, 2);
										polygon_pad_connection := to_pad_connection (f (line, 2));
										
									elsif kw = keyword_thermal_width then -- thermal_width 0.3
										expect_field_count (line, 2);
										pcb_rw.thermal.width := to_distance (f (line, 2));

									elsif kw = keyword_thermal_gap then -- thermal_gap 0.7
										expect_field_count (line, 2);
										pcb_rw.thermal.gap := to_distance (f (line, 2));

									else
										invalid_keyword (kw);
									end if;
								end;

							when SEC_TOP | SEC_BOTTOM => 
								case stack.parent (degree => 2) is
									when SEC_SILK_SCREEN | SEC_ASSEMBLY_DOCUMENTATION |
										SEC_STENCIL | SEC_STOP_MASK =>
										declare
											use et_packages;
											use et_packages.pac_shapes;
											use et_pcb_coordinates.geometry;
											kw : string := f (line, 1);
										begin
											-- CS: In the following: set a corresponding parameter-found-flag
											if kw = keyword_fill_style then -- fill_style solid/hatched
												expect_field_count (line, 2);													
												board_fill_style := to_fill_style (f (line, 2));

											elsif kw = keyword_corner_easing then -- corner_easing none/chamfer/fillet
												expect_field_count (line, 2);													
												board_easing.style := to_corner_easing (f (line, 2));

											elsif kw = keyword_easing_radius then -- easing_radius 0.4
												expect_field_count (line, 2);													
												board_easing.radius := to_distance (f (line, 2));
												
											elsif kw = keyword_hatching_line_width then -- hatching_line_width 0.3
												expect_field_count (line, 2);													
												board_hatching.line_width := to_distance (f (line, 2));

											elsif kw = keyword_hatching_line_spacing then -- hatching_line_spacing 0.3
												expect_field_count (line, 2);													
												board_hatching.spacing := to_distance (f (line, 2));
												
											else
												invalid_keyword (kw);
											end if;
										end;

									when SEC_KEEPOUT =>
										declare
											use et_packages.pac_shapes;
											kw : string := f (line, 1);
										begin
											-- CS: In the following: set a corresponding parameter-found-flag
											if kw = keyword_filled then -- filled yes/no
												expect_field_count (line, 2);													
												board_filled := to_filled (f (line, 2));

											else
												invalid_keyword (kw);
											end if;
										end;
										
									when others => invalid_section;
								end case;

							when SEC_ROUTE_RESTRICT | SEC_VIA_RESTRICT =>
								declare
									use et_pcb_stack;
									use et_packages;
									use et_packages.pac_shapes;
									use et_pcb_coordinates.geometry;
									kw : string := f (line, 1);
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_filled then -- filled yes/no
										expect_field_count (line, 2);													
										board_filled := to_filled (f (line, 2));

									elsif kw = keyword_layers then -- layers 1 14 3

										-- there must be at least two fields:
										expect_field_count (line => line, count_expected => 2, warn => false);
										signal_layers := to_layers (line);

									else
										invalid_keyword (kw);
									end if;
								end;

							when SEC_COPPER => -- non electrical
								declare
									use et_packages;
									use et_packages.pac_shapes;									
									use et_pcb_stack;
									use et_pcb_coordinates.geometry;
									kw : string := f (line, 1);
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_fill_style then -- fill_style solid/hatched
										expect_field_count (line, 2);													
										board_fill_style := to_fill_style (f (line, 2));

									elsif kw = keyword_corner_easing then -- corner_easing none/chamfer/fillet
										expect_field_count (line, 2);													
										board_easing.style := to_corner_easing (f (line, 2));

									elsif kw = keyword_easing_radius then -- easing_radius 0.4
										expect_field_count (line, 2);													
										board_easing.radius := to_distance (f (line, 2));
										
									elsif kw = keyword_hatching_line_width then -- hatching_line_width 0.3
										expect_field_count (line, 2);													
										board_hatching_copper.line_width := to_distance (f (line, 2));

									elsif kw = keyword_hatching_line_spacing then -- hatching_line_spacing 0.3
										expect_field_count (line, 2);													
										board_hatching_copper.spacing := to_distance (f (line, 2));

									elsif kw = keyword_hatching_border_width then -- hatching_border_width 1
										expect_field_count (line, 2);													
										board_hatching_copper.border_width := to_distance (f (line, 2));
										
									elsif kw = keyword_min_width then -- min_width 0.5
										expect_field_count (line, 2);
										polygon_width_min := to_distance (f (line, 2));
										
									elsif kw = keyword_layer then -- layer 1
										expect_field_count (line, 2);
										signal_layer := et_pcb_stack.to_signal_layer (f (line, 2));

									elsif kw = et_pcb.keyword_priority then -- priority 2
										expect_field_count (line, 2);
										polygon_priority := et_pcb.to_polygon_priority (f (line, 2));

									elsif kw = keyword_isolation then -- isolation 0.5
										expect_field_count (line, 2);
										polygon_isolation := to_distance (f (line, 2));
										
									else
										invalid_keyword (kw);
									end if;
								end;
								
							when others => invalid_section;
						end case;

					when SEC_VIA =>
						case stack.parent is
							when SEC_ROUTE =>
								declare
									use et_pcb_coordinates.geometry;
									use et_pcb;
									use et_packages;
									kw : string := f (line, 1);
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_position then -- position x 22.3 y 23.3
										expect_field_count (line, 5);

										-- extract the position starting at field 2 of line
										route_via.position := to_position (line, 2);

									elsif kw = keyword_diameter then -- diameter 0.35
										expect_field_count (line, 2);
										route_via.diameter := to_distance (f (line, 2));

									elsif kw = keyword_restring_outer_layers then -- restring_outer_layers 0.3
										expect_field_count (line, 2);
										route_via.restring_outer := to_distance (f (line, 2));

									elsif kw = keyword_restring_inner_layers then -- restring_inner_layers 0.34
										expect_field_count (line, 2);
										route_via.restring_inner := to_distance (f (line, 2));

									elsif kw = keyword_layer_start then -- layer_start 1
										expect_field_count (line, 2);
										route_via.layer_start := et_pcb_stack.to_signal_layer (f (line, 2));

									elsif kw = keyword_layer_end then -- layer_end 15
										expect_field_count (line, 2);
										route_via.layer_end := et_pcb_stack.to_signal_layer (f (line, 2));
										
									else
										invalid_keyword (kw);
									end if;
								end;

							when others => invalid_section;
						end case;
					
					when SEC_SUBMODULE =>
						case stack.parent is
							when SEC_SUBMODULES =>
								declare
									use submodules;
									kw : string := f (line, 1);
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_file then -- file $ET_TEMPLATES/motor_driver.mod
										expect_field_count (line, 2);
										submodule.file := submodules.to_submodule_path (f (line, 2));

									elsif kw = keyword_name then -- name stepper_driver
										expect_field_count (line, 2);
										submodule_name := to_instance_name (f (line, 2));

									elsif kw = keyword_position then -- position sheet 3 x 130 y 210
										expect_field_count (line, 7);

										-- extract position of submodule starting at field 2
										submodule.position := to_position (line, 2);

									elsif kw = submodules.keyword_size then -- size x 30 y 30
										expect_field_count (line, 5);

										-- extract size of submodule starting at field 2
										submodule.size := to_size (line, 2);

									elsif kw = keyword_position_in_board then -- position_in_board x 23 y 0.2 rotation 90.0
										expect_field_count (line, 7);

										-- extract position of submodule starting at field 2
										submodule.position_in_board := to_position (line, 2);

									elsif kw = keyword_view_mode then -- view_mode origin/instance
										expect_field_count (line, 2);
										submodule.view_mode := submodules.to_view_mode (f (line, 2));

									else
										invalid_keyword (kw);
									end if;
								end;
								
							when others => invalid_section;
						end case;

					when SEC_PORT =>
						case stack.parent is
							when SEC_PORTS =>
								case stack.parent (degree => 2) is
									when SEC_SUBMODULE =>
										declare
											kw : string := f (line, 1);
										begin
											-- CS: In the following: set a corresponding parameter-found-flag
											if kw = keyword_name then -- name clk_out
												expect_field_count (line, 2);
												submodule_port_name := to_net_name (f (line, 2));

											elsif kw = keyword_position then -- position x 0 y 10
												expect_field_count (line, 5);

												-- extract port position starting at field 2
												submodule_port.position := to_position (line, 2);

											elsif kw = submodules.keyword_direction then -- direction master/slave
												expect_field_count (line, 2);

												submodule_port.direction := submodules.to_port_name (f (line, 2));
											else
												invalid_keyword (kw);
											end if;
										end;

									when others => invalid_section;
								end case;

							when others => invalid_section;
						end case;
						
					when SEC_SCHEMATIC =>
						case stack.parent is
							when SEC_DRAWING_FRAMES => read_frame_template_schematic;
							when SEC_DRAWING_GRID => read_drawing_grid_schematic;
							when SEC_META => read_meta_schematic;
							when others => invalid_section;
						end case;

					when SEC_BOARD =>
						case stack.parent is
							when SEC_INIT => null; -- nothing to do
							when SEC_DRAWING_FRAMES => read_frame_template_board;
							when SEC_DRAWING_GRID => read_drawing_grid_board;
							when SEC_META => read_meta_board;
							when others => invalid_section;
						end case;
						
					when SEC_TEXT =>
						case stack.parent is
							when SEC_TEXTS => -- in schematic
								declare
									use et_coordinates.geometry;
									kw : string := f (line, 1);
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_position then -- position sheet 2 x 91.44 y 118.56
										expect_field_count (line, 7);

										-- extract position of note starting at field 2
										note.position := to_position (line, 2);

									elsif kw = keyword_content then -- content "DUMMY TEXT IN CORE MODULE"
										expect_field_count (line, 2); -- actual content in quotes !
										note.content := et_text.to_content (f (line, 2));

									elsif kw = et_text.keyword_size then -- size 1.4
										expect_field_count (line, 2);
										note.size := to_distance (f (line, 2));

									elsif kw = et_text.keyword_line_width then -- line_width 0.1
										expect_field_count (line, 2);
										note.line_width := to_distance (f (line, 2));

									elsif kw = keyword_rotation then -- rotation 90
										expect_field_count (line, 2);
										note.rotation := to_rotation (f (line, 2));

									elsif kw = keyword_style then -- stlye normal/italic
										expect_field_count (line, 2);
										note.style := et_symbols.to_text_style (f (line, 2));

									elsif kw = et_text.keyword_alignment then -- alignment horizontal center vertical center
										expect_field_count (line, 5);

										-- extract alignment starting at field 2
										note.alignment := et_text.to_alignment (line, 2);
										
									else
										invalid_keyword (kw);
									end if;
								end;

							when SEC_TOP | SEC_BOTTOM =>
								case stack.parent (degree => 2) is
									when SEC_SILK_SCREEN | SEC_ASSEMBLY_DOCUMENTATION | SEC_STOP_MASK => -- CS SEC_KEEPOUT
										declare
											use et_pcb_coordinates.geometry;
											kw : string := f (line, 1);
										begin
											-- CS: In the following: set a corresponding parameter-found-flag
											if kw = keyword_position then -- position x 91.44 y 118.56 rotation 45.0
												expect_field_count (line, 7);

												-- extract position of note starting at field 2
												board_text.position := to_position (line, 2);

											elsif kw = et_text.keyword_size then -- size 1.000
												expect_field_count (line, 2);
												board_text.size := to_distance (f (line, 2));

											elsif kw = et_text.keyword_line_width then -- line_width 0.1
												expect_field_count (line, 2);
												board_text.line_width := to_distance (f (line, 2));

											elsif kw = et_text.keyword_alignment then -- alignment horizontal center vertical center
												expect_field_count (line, 5);

												-- extract alignment starting at field 2
												board_text.alignment := et_text.to_alignment (line, 2);
												
											elsif kw = keyword_content then -- content "WATER KETTLE CONTROL"
												expect_field_count (line, 2); -- actual content in quotes !
												board_text.content := et_text.to_content (f (line, 2));
												
											else
												invalid_keyword (kw);
											end if;
										end;
										
									when others => invalid_section;
								end case;

							when SEC_COPPER =>
								declare
									use et_pcb_coordinates.geometry;
									use et_pcb_stack;
									kw : string := f (line, 1);
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_position then -- position x 91.44 y 118.56 rotation 45.0
										expect_field_count (line, 7);

										-- extract position of note starting at field 2
										board_text_copper.position := to_position (line, 2);

									elsif kw = et_text.keyword_size then -- size 1.000
										expect_field_count (line, 2);
										board_text_copper.size := to_distance (f (line, 2));

									elsif kw = et_text.keyword_line_width then -- line_width 0.1
										expect_field_count (line, 2);
										board_text_copper.line_width := to_distance (f (line, 2));

									elsif kw = et_text.keyword_alignment then -- alignment horizontal center vertical center
										expect_field_count (line, 5);

										-- extract alignment starting at field 2
										board_text_copper.alignment := et_text.to_alignment (line, 2);
										
									elsif kw = keyword_content then -- content "TOP", "L2", "BOT"
										expect_field_count (line, 2); -- actual content in quotes !
										board_text_copper.content := et_text.to_content (f (line, 2));

									elsif kw = keyword_layer then -- layer 15
										expect_field_count (line, 2);
										board_text_copper.layer := et_pcb_stack.to_signal_layer (f (line, 2));
										
									else
										invalid_keyword (kw);
									end if;
								end;
								
							when others => invalid_section;
						end case;

					when SEC_DEVICE =>
						case stack.parent is
							when SEC_DEVICES =>
								declare
									use et_symbols;
									use et_devices;
									kw : string := f (line, 1);
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_name then -- name C12
										expect_field_count (line, 2);
										device_name := to_name (f (line, 2));

									-- As soon as the appearance becomes clear, a temporarily device is
									-- created where pointer "device" is pointing at:
									elsif kw = keyword_appearance then -- sch_pcb, sch
										expect_field_count (line, 2);
										device_appearance := to_appearance (f (line, 2));

										case device_appearance is
											when VIRTUAL =>
												device := new et_schematic.type_device'(
													appearance	=> VIRTUAL,
													others		=> <>);

											when PCB =>
												device := new et_schematic.type_device'(
													appearance	=> PCB,
													others		=> <>);
										end case;
												
									elsif kw = keyword_value then -- value 100n
										expect_field_count (line, 2);

										-- validate value
										device_value := to_value (f (line, 2));

									elsif kw = keyword_model then -- model /models/capacitor.dev
										expect_field_count (line, 2);
										device_model := to_file_name (f (line, 2));
										
									elsif kw = keyword_variant then -- variant S_0805, N, D
										expect_field_count (line, 2);
										check_variant_name_length (f (line, 2));
										device_variant := to_name (f (line, 2));

									elsif kw = material.keyword_partcode then -- partcode LED_PAC_S_0805_VAL_red
										expect_field_count (line, 2);

										-- validate partcode
										device_partcode := material.to_partcode (f (line, 2));

									elsif kw = keyword_purpose then -- purpose power_out
										expect_field_count (line, 2);

										-- validate purpose
										device_purpose := to_purpose (f (line, 2));
									else
										invalid_keyword (kw);
									end if;
								end;
									
							when others => invalid_section;
						end case;

					when SEC_PACKAGE =>
						case stack.parent is
							when SEC_DEVICE =>
								declare
									kw : string := f (line, 1);
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_position then -- position x 163.500 y 92.500 rotation 0.00 face top
										expect_field_count (line, 9);

										-- extract package position starting at field 2
										device_position := to_position (line, 2);

									elsif kw = keyword_flipped then -- flipped no/yes
										expect_field_count (line, 2);

										device_flipped := et_pcb.to_flipped (f (line, 2));
									else
										invalid_keyword (kw);
									end if;
								end;

							when others => invalid_section;
						end case;

					when SEC_PLACEHOLDER =>
						case stack.parent is
							when SEC_PLACEHOLDERS =>
								case stack.parent (degree => 2) is
									when SEC_PACKAGE => -- in layout
										declare
											use et_packages;
											use et_pcb_stack;
											use et_pcb_coordinates.geometry;
											kw : string := f (line, 1);
										begin
											-- CS: In the following: set a corresponding parameter-found-flag
											if kw = keyword_meaning then -- meaning reference, value, ...
												expect_field_count (line, 2);
												device_text_placeholder.meaning := to_text_meaning (f (line, 2));
												
											elsif kw = keyword_layer then -- layer silk_screen/assembly_documentation
												expect_field_count (line, 2);
												device_text_placeholder_layer := to_layer (f (line, 2));
												
											elsif kw = keyword_position then -- position x 0.000 y 5.555 rotation 0.00 face top
												expect_field_count (line, 9);

												-- extract position of placeholder starting at field 2
												device_text_placeholder_position := to_position (line, 2);

											elsif kw = et_text.keyword_size then -- size 5
												expect_field_count (line, 2);
												device_text_placeholder.size := to_distance (f (line, 2));

											elsif kw = et_text.keyword_line_width then -- line_width 0.15
												expect_field_count (line, 2);

												device_text_placeholder.line_width := to_distance (f (line, 2));

											elsif kw = et_text.keyword_alignment then -- alignment horizontal center vertical center
												expect_field_count (line, 5);

												-- extract alignment of placeholder starting at field 2
												device_text_placeholder.alignment := et_text.to_alignment (line, 2);
												
											else
												invalid_keyword (kw);
											end if;
										end;

									when SEC_UNIT =>
										declare
											use et_coordinates.geometry;
											kw : string := f (line, 1);
										begin
											-- CS: In the following: set a corresponding parameter-found-flag
											if kw = keyword_meaning then -- meaning reference, value or purpose
												expect_field_count (line, 2);
												unit_placeholder_meaning := et_symbols.to_meaning (f (line, 2));
												
											elsif kw = keyword_position then -- position x 0.000 y 5.555
												expect_field_count (line, 5);

												-- extract position of placeholder starting at field 2
												unit_placeholder_position := to_position (line, 2);

											elsif kw = et_text.keyword_size then -- size 3.0
												expect_field_count (line, 2);
												unit_placeholder.size := to_distance (f (line, 2));

											elsif kw = et_text.keyword_line_width then -- line_width 0.15
												expect_field_count (line, 2);

												unit_placeholder.line_width := to_distance (f (line, 2));

											elsif kw = keyword_rotation then -- rotation 90.0
												expect_field_count (line, 2);

												unit_placeholder.rotation := to_rotation (f (line, 2));

											elsif kw = keyword_style then -- stlye italic
												expect_field_count (line, 2);

												unit_placeholder.style := et_symbols.to_text_style (f (line, 2));

											elsif kw = et_text.keyword_alignment then -- alignment horizontal center vertical center
												expect_field_count (line, 5);

												-- extract alignment of placeholder starting at field 2
												unit_placeholder.alignment := et_text.to_alignment (line, 2);
												
											else
												invalid_keyword (kw);
											end if;
										end;

									when others => invalid_section;
								end case;

							when SEC_TOP | SEC_BOTTOM =>
								case stack.parent (degree => 2) is
									when SEC_SILK_SCREEN | SEC_ASSEMBLY_DOCUMENTATION | SEC_STOP_MASK => -- CS SEC_KEEPOUT
										declare
											use et_pcb_coordinates.geometry;
											kw : string := f (line, 1);
										begin
											-- CS: In the following: set a corresponding parameter-found-flag
											if kw = keyword_position then -- position x 91.44 y 118.56 rotation 45.0
												expect_field_count (line, 7);

												-- extract position of note starting at field 2
												board_text_placeholder.position := to_position (line, 2);

											elsif kw = et_text.keyword_size then -- size 1.000
												expect_field_count (line, 2);
												board_text_placeholder.size := to_distance (f (line, 2));

											elsif kw = et_text.keyword_line_width then -- line_width 0.1
												expect_field_count (line, 2);
												board_text_placeholder.line_width := to_distance (f (line, 2));

											elsif kw = et_text.keyword_alignment then -- alignment horizontal center vertical center
												expect_field_count (line, 5);

												-- extract alignment starting at field 2
												board_text_placeholder.alignment := et_text.to_alignment (line, 2);
												
											elsif kw = keyword_meaning then -- meaning project_name
												expect_field_count (line, 2);
												board_text_placeholder.meaning := et_pcb.to_meaning (f (line, 2));
												
											else
												invalid_keyword (kw);
											end if;
										end;
										
									when others => invalid_section;
								end case;

							when SEC_COPPER =>
								declare
									use et_pcb_coordinates.geometry;
									use et_pcb_stack;
									kw : string := f (line, 1);
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_position then -- position x 91.44 y 118.56 rotation 45.0
										expect_field_count (line, 7);

										-- extract position of note starting at field 2
										board_text_copper_placeholder.position := to_position (line, 2);

									elsif kw = et_text.keyword_size then -- size 1.000
										expect_field_count (line, 2);
										board_text_copper_placeholder.size := to_distance (f (line, 2));

									elsif kw = et_text.keyword_line_width then -- line_width 0.1
										expect_field_count (line, 2);
										board_text_copper_placeholder.line_width := to_distance (f (line, 2));

									elsif kw = et_text.keyword_alignment then -- alignment horizontal center vertical center
										expect_field_count (line, 5);

										-- extract alignment starting at field 2
										board_text_copper_placeholder.alignment := et_text.to_alignment (line, 2);
										
									elsif kw = keyword_meaning then -- meaning revision/project_name/...
										expect_field_count (line, 2);
										board_text_copper_placeholder.meaning := et_pcb.to_meaning (f (line, 2));

									elsif kw = keyword_layer then -- layer 15
										expect_field_count (line, 2);
										board_text_copper_placeholder.layer := et_pcb_stack.to_signal_layer (f (line, 2));
										
									else
										invalid_keyword (kw);
									end if;
								end;
								
							when others => invalid_section;
						end case;

					when SEC_PLACEHOLDERS =>
						case stack.parent is
							when SEC_PACKAGE => null;
							when SEC_UNIT => null;
							when others => invalid_section;
						end case;

					when SEC_UNIT =>
						case stack.parent is
							when SEC_UNITS =>
								declare
									use et_devices;
									kw : string := f (line, 1);
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_name then -- name 1, GPIO_BANK_1, ...
										expect_field_count (line, 2);
										device_unit_name := to_name (f (line, 2));
										
									elsif kw = keyword_position then -- position sheet 1 x 1.000 y 5.555
										expect_field_count (line, 7);

										-- extract position of unit starting at field 2
										device_unit_position := to_position (line, 2);

									elsif kw = keyword_rotation then -- rotation 180.0
										expect_field_count (line, 2);
										--device_unit_rotation := geometry.to_rotation (f (line, 2));
										set (device_unit_position, geometry.to_rotation (f (line, 2)));

									elsif kw = keyword_mirrored then -- mirrored no/x_axis/y_axis
										expect_field_count (line, 2);
										device_unit_mirror := et_schematic.to_mirror_style (f (line, 2));

									else
										invalid_keyword (kw);
									end if;
								end;
								
							when others => invalid_section;
						end case;

					when SEC_UNITS =>
						case stack.parent is
							when SEC_DEVICE => null;
							when others => invalid_section;
						end case;

					when SEC_NETCHANGERS =>
						case stack.parent is
							when SEC_INIT => null; -- nothing to do
							when others => invalid_section;
						end case;

					when SEC_NETCHANGER =>
						case stack.parent is
							when SEC_NETCHANGERS =>
								declare
									kw : string := f (line, 1);
									use et_pcb_stack;
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_name then -- name 1, 2, 304, ...
										expect_field_count (line, 2);
										netchanger_id := submodules.to_netchanger_id (f (line, 2));
										
									elsif kw = keyword_position_in_schematic then -- position_in_schematic sheet 1 x 1.000 y 5.555
										expect_field_count (line, 7);

										-- extract position (in schematic) starting at field 2
										netchanger.position_sch := to_position (line, 2);

									elsif kw = keyword_rotation_in_schematic then -- rotation_in_schematic 180.0
										expect_field_count (line, 2);
										set (netchanger.position_sch, geometry.to_rotation (f (line, 2)));

									elsif kw = keyword_position_in_board then -- position_in_board x 55.000 y 7.555
										expect_field_count (line, 5);

										-- extract position (in board) starting at field 2
										netchanger.position_brd := to_position (line, 2);

									elsif kw = keyword_layer then -- layer 3 (signal layer in board)
										expect_field_count (line, 2);
										netchanger.layer := et_pcb_stack.to_signal_layer (f (line, 2));
										
									else
										invalid_keyword (kw);
									end if;
								end;

							when others => invalid_section;
						end case;
						
					when SEC_SILK_SCREEN | SEC_ASSEMBLY_DOCUMENTATION | SEC_STENCIL |
						SEC_STOP_MASK | SEC_KEEPOUT | SEC_ROUTE_RESTRICT | SEC_VIA_RESTRICT |
						SEC_COPPER | SEC_PCB_CONTOURS_NON_PLATED =>
						case stack.parent is
							when SEC_BOARD => null;
							when others => invalid_section;
						end case;

					when SEC_TOP | SEC_BOTTOM =>
						case stack.parent is
							when SEC_SILK_SCREEN | SEC_ASSEMBLY_DOCUMENTATION | SEC_STENCIL |
								SEC_STOP_MASK | SEC_KEEPOUT => null;

							when others => invalid_section;
						end case;
						
					when SEC_INIT => null; -- CS: should never happen
				end case;
			end if;

			exception when event: others =>
				log (text => "file " & file_name & space 
					& affected_line (line) & to_string (line), console => true);
				raise;
			
		end process_line;

		procedure read_submodule_files is
		-- Pointer module_cursor points to the last module that has been read.
		-- Take a copy of the submodules stored in module.submods. 
		-- Then iterate in that copy (submods) to read the actual 
		-- module files (like templates/clock_generator.mod).
		-- NOTE: The parent procedure "read_module_file" calls itself here !

			use submodules;
			use type_submodules;

			-- Here the copy of submodules lives:
			submods : submodules.type_submodules.map;
			
			procedure get_submodules (
			-- Copies the submodules in submods.
				module_name	: type_module_name.bounded_string;
				module		: et_schematic.type_module) is
			begin
				submods := module.submods;
			end;

			procedure query_module (cursor : in type_submodules.cursor) is begin
				-- Read the template file:
				read_module_file (to_string (element (cursor).file), log_threshold + 1);
			end;
			
		begin -- read_submodule_files
			-- take a copy of submodules
			query_element (
				position	=> module_cursor,
				process		=> get_submodules'access);

			if length (submods) > 0 then
				log (text => "submodules/templates ...", level => log_threshold);
				log_indentation_up;
			
				-- Query submodules of the parent module (accessed by module_cursor):
				iterate (submods, query_module'access);

				log_indentation_down;
			end if;

		end read_submodule_files;

		procedure test_assembly_variants_of_submodules is
		-- Tests whether the submodules provides the assembly variants as 
		-- specified in module file section ASSEMBLY_VARIANTS.

			procedure query_variants (
				module_name	: in type_module_name.bounded_string;
				module		: in et_schematic.type_module) is

				use assembly_variants;
				use assembly_variants.pac_variants;
				
				variant_cursor : assembly_variants.pac_variants.cursor := module.variants.first;
				variant_name : et_general.type_variant_name.bounded_string; -- low_cost

				procedure query_submodules (
					variant_name	: in et_general.type_variant_name.bounded_string;
					variant			: in assembly_variants.type_variant) is
					use type_submodules;
					submod_cursor : type_submodules.cursor := variant.submodules.first;
					submod_name : type_module_instance_name.bounded_string; -- CLK_GENERATOR
					submod_variant : et_general.type_variant_name.bounded_string; -- fixed_frequency
				begin -- query_submodules
					if submod_cursor = type_submodules.no_element then
						log (text => "no submodule variants specified", level => log_threshold + 1);
					else
						-- iterate variants of submodules
						while submod_cursor /= type_submodules.no_element loop
							submod_name := key (submod_cursor); -- CLK_GENERATOR
							submod_variant := element (submod_cursor).variant;
							
							log (text => "submodule instance " & 
								 enclose_in_quotes (to_string (submod_name)) &
								 " variant " & 
								 enclose_in_quotes (to_variant (submod_variant)),
								 level => log_threshold + 2);

							if not exists (module_cursor, submod_name, submod_variant) then
								log (ERROR, "submodule " &
									enclose_in_quotes (to_string (submod_name)) &
									" does not provide assembly variant " &
									enclose_in_quotes (to_variant (submod_variant)) & " !",
									console => true);

								log (text => "Look up section " & section_assembly_variants (2..section_assembly_variants'last) &
									 " to fix the issue !");
								
								raise constraint_error;
							end if;

							next (submod_cursor);
						end loop;
					end if;
				end query_submodules;
				
			begin -- query_variants
				if variant_cursor = assembly_variants.pac_variants.no_element then
					log (text => "no variants specified", level => log_threshold);
				else
					-- iterate assembly variants of parent module
					while variant_cursor /= assembly_variants.pac_variants.no_element loop
						variant_name := key (variant_cursor);

						-- show assembly variant of parent module
						log (text => "variant " & enclose_in_quotes (to_variant (variant_name)), level => log_threshold + 1);
						log_indentation_up;

						-- look up the submodule variants
						query_element (
							position	=> variant_cursor,
							process		=> query_submodules'access);

						log_indentation_down;
						
						next (variant_cursor);
					end loop;
				end if;
			end;
			
		begin -- test_assembly_variants_of_submodules
			log (text => "verifying assembly variants of submodules ...", level => log_threshold);
			log_indentation_up;

			query_element (
				position	=> module_cursor,
				process		=> query_variants'access);

			log_indentation_down;
		end test_assembly_variants_of_submodules;

		
		use ada.directories;
		
	begin -- read_module_file
		log (text => "opening file " & file_name & " ...", level => log_threshold);
		log_indentation_up;

		-- Make sure the module file exists.
		-- The file is identified by its full path and name.
		if exists (full_file_name) then

			log (text => "expanded name: " & full_name (full_file_name), level => log_threshold + 1);
			
			-- Create an empty module named after the module file (omitting extension *.mod).
			-- So the module names are things like "motor_driver", "templates/clock_generator" or
			-- "$TEMPLATES/clock_generator" or "/home/user/templates/clock_generator":
			type_modules.insert (
				container	=> modules,
				key			=> to_module_name (remove_extension (file_name)),
				position	=> module_cursor,
				inserted	=> module_inserted);

			-- If the module is new, then open the file and read it. 
			-- Otherwise notify operator that module has already been loaded.			 
			if module_inserted then
				
				-- open module file
				open (
					file => file_handle,
					mode => in_file, 
					name => full_file_name); -- full path and name
				
				set_input (file_handle);
				
				-- Init section stack.
				stack.init;
				stack.push (SEC_INIT);
				
				-- read the file line by line
				while not end_of_file loop
					line := et_string_processing.read_line (
						line 			=> get_line,
						number			=> ada.text_io.line (current_input),
						comment_mark 	=> comment_mark,
						delimiter_wrap	=> true, -- strings are enclosed in quotations
						ifs 			=> space); -- fields are separated by space

					-- we are interested in lines that contain something. emtpy lines are skipped:
					if field_count (line) > 0 then
						process_line;
					end if;
				end loop;

				-- As a safety measure the top section must be reached finally.
				if stack.depth > 1 then 
					log (text => message_warning & write_section_stack_not_empty);
				end if;

				set_input (previous_input);
				close (file_handle);

				-- Pointer module_cursor points to the last module that has been read.		
				-- The names of submodule/template files are stored in module.submods.file.
				-- But the files itself have not been read. That is what we do next:
				read_submodule_files;

				-- Test existence of assembly variants of submodules.
				test_assembly_variants_of_submodules;
				
			else
				log (text => "module " & enclose_in_quotes (file_name) &
					 " already loaded -> no need to load anew.", level => log_threshold + 1);
			end if;
			
		else -- module file not found
			log (ERROR, "module file " & enclose_in_quotes (file_name) &
				 " not found !", console => true);
			raise constraint_error;
		end if;

		log_indentation_down;
		
		exception when event: others =>
			if is_open (file_handle) then close (file_handle); end if;
			set_input (previous_input);
			raise;
		
	end read_module_file;

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
	

end et_project;
	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
