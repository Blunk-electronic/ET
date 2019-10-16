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
with ada.characters.latin_1;	use ada.characters.latin_1;
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
with et_libraries;
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

package body et_project is

	use et_general.type_net_name;
	
	function expand (
	-- Translates a file name like $HOME/libraries/devices/7400.dev to
	-- /home/user/libraries/devices/7400.dev
	-- CS: works on unix/linux only
		name_in			: in string) -- $HOME/libraries/devices/7400.dev
		--log_threshold	: et_string_processing.type_log_level)
		return string is

		prefix : constant string := ("$"); -- CS: windows ? (like %home%)
		separator : constant string := (1 * gnat.directory_operations.dir_separator); -- /\
		
		place_prefix, place_separator : natural := 0;
		--use gnat.directory_operations;
		--use et_string_processing;
		
-- 		function do_it (path : in string) return string is begin
-- 			log ("full path is " & path, log_threshold + 1);
-- 			return path;
-- 		end;
		
	begin -- expand
		place_prefix := index (name_in, prefix);
		place_separator := index (name_in, separator);

		if place_prefix = 0 then -- no environment variable found
			return name_in; -- return given name as it is
		else
			-- name contains an environment variable
			--log_indentation_up;
			
			--log ("expanding " & name_in, log_threshold);
			-- CS test_vars (name_in); -- test if environment variables exist
			
			--log_indentation_down;
			
			--return do_it (gnat.directory_operations.expand_path (name_in));
			return gnat.directory_operations.expand_path (name_in);
		end if;

	end expand;
	
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

	procedure invalid_keyword (word : in string) is 
		use et_string_processing;
	begin
		log (ERROR, "invalid keyword '" & word & "' !", console => true);
		raise constraint_error;
	end;
	
	function to_grid (
		line : in et_string_processing.type_fields_of_line; -- "default x 1 y 1"
		from : in positive)
		return et_coordinates.geometry.type_grid is
		use et_string_processing;
		use et_coordinates.geometry;
		
		grid : et_coordinates.geometry.type_grid; -- to be returned

		place : positive := from; -- the field being read from given line

		function f (line : in type_fields_of_line; position : in positive) return string 
			renames et_string_processing.field;
		
	begin
		while place <= positive (field_count (line)) loop

			-- We expect after the x the corresponding value for x
			if f (line, place) = keyword_pos_x then
				grid.x := to_distance (f (line, place + 1));

			-- We expect after the y the corresponding value for y
			elsif f (line, place) = keyword_pos_y then
				grid.y := to_distance (f (line, place + 1));

			else
				invalid_keyword (f (line, place));
			end if;
					
			place := place + 2;
		end loop;
		
		return grid;
	end to_grid;

	function to_grid (
		line : in et_string_processing.type_fields_of_line; -- "default x 1 y 1"
		from : in positive)
		return et_pcb_coordinates.geometry.type_grid is
		use et_string_processing;
		use et_pcb_coordinates.geometry;
		
		grid : et_pcb_coordinates.geometry.type_grid; -- to be returned

		place : positive := from; -- the field being read from given line

		function f (line : in type_fields_of_line; position : in positive) return string 
			renames et_string_processing.field;
		
	begin
		while place <= positive (field_count (line)) loop

			-- We expect after the x the corresponding value for x
			if f (line, place) = keyword_pos_x then
				grid.x := to_distance (f (line, place + 1));

			-- We expect after the y the corresponding value for y
			elsif f (line, place) = keyword_pos_y then
				grid.y := to_distance (f (line, place + 1));

			else
				invalid_keyword (f (line, place));
			end if;
					
			place := place + 2;
		end loop;
		
		return grid;
	end to_grid;
	
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
		use et_libraries.type_device_purpose;
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
	
	procedure tab_depth_up is begin tab_depth := tab_depth + 1; end tab_depth_up;
	procedure tab_depth_down is begin tab_depth := tab_depth - 1; end tab_depth_down;
	procedure reset_tab_depth is begin tab_depth := type_tab_depth'first; end reset_tab_depth;

	procedure section_mark (section : in string; mark : in type_section_mark) is begin
	-- Make sure the current_output is set properly.
		case mark is
			when HEADER =>
				--new_line;
				put_line (tab_depth * tab & section & space & section_begin);
				tab_depth_up;
			when FOOTER =>
				tab_depth_down;
				put_line (tab_depth * tab & section & space & section_end);
		end case;
	end section_mark;

	procedure write (
		keyword 	: in string;
		parameters	: in string;
		space 		: in boolean := false;
		wrap		: in boolean := false) is 
		parameters_wrapped : string (1..parameters'length + 2);
	begin -- write
		if wrap then
			parameters_wrapped := latin_1.quotation & parameters & latin_1.quotation;
		end if;
					
		if wrap then
			-- If wrapping required, a space is always between keyword and parameters
			put_line (tab_depth * tab & keyword & latin_1.space & parameters_wrapped);
		else
			case space is
				when true =>
					put_line (tab_depth * tab & keyword & latin_1.space & parameters);
				when false =>
					put_line (tab_depth * tab & keyword & parameters);
			end case;
		end if;
	end write;	

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
			write (keyword => keyword_generic_name, space => true, parameters => to_string (project_name));
			write (keyword => keyword_instance_name, space => true, parameters => to_string (project_name));
			section_mark (section_module, FOOTER);			
			
			-- CS In the future, write other things here that characterize the instance.
			section_mark (section_module_instances, FOOTER);


			-- section connectors
			new_line;
			section_mark (section_module_connections, HEADER);

			section_mark (section_connector, HEADER);			
			write (keyword => comment_mark & " " & keyword_instance_A, space => true, parameters => to_string (project_name));
			write (keyword => comment_mark & " " & keyword_purpose_A, space => true, wrap => true, parameters => "power_in");
			new_line;
			write (keyword => comment_mark & " " & keyword_instance_B, space => true, parameters => "power_supply");
			write (keyword => comment_mark & " " & keyword_purpose_B, space => true, wrap => true, parameters => "power_out");
			new_line;
			write (keyword => comment_mark & " " & keyword_net_comparator, space => true, parameters => "on"); -- CS image of enum type
			write (keyword => comment_mark & " " & keyword_net_comparator_warn_only, space => true, parameters => "on"); -- CS image of enum type
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

	function position (pos : in et_coordinates.geometry.type_point'class) return string is
	-- Returns something like "x 12.34 y 45.0" or "sheet 3 x 12.34 y 45.0".
	-- This kind of output depends on the tag of the given object.
		use et_coordinates.geometry;
		use ada.tags;

		-- This function returns the basic text with x and y coordinates.
		function text return string is begin return 
			space & keyword_pos_x & to_string (x (pos)) 
			& space & keyword_pos_y & to_string (y (pos));
		end text;
		
	begin -- position
		if pos'tag = type_point'tag then
			return text; -- a 2d point has just x and y
		else
			-- A type_coordinates also has the sheet number:
			return space & keyword_sheet & to_sheet (sheet (et_coordinates.type_position (pos))) & text;
		end if;
	end position;

-- 	function rotation (angle : in et_coordinates.type_rotation) return string is 
-- 	begin
-- 		if angle < zero_rotation then
-- 			return latin_1.space & et_coordinates.type_rotation'image (angle);
-- 		else
-- 			return et_coordinates.type_rotation'image (angle);
-- 		end if;
-- 	end rotation;

	function position (point : et_pcb_coordinates.geometry.type_point'class) return string is
		use et_pcb_coordinates;
		use et_pcb_coordinates.geometry;
		use ada.tags;

		xy : constant string := space & keyword_pos_x & to_string (x (point)) 
				& space & keyword_pos_y & to_string (y (point));
	begin
		if point'tag = et_pcb_coordinates.geometry.type_point'tag then
			return xy;
			-- position x 162.560 y 98.240
			
		elsif point'tag = et_pcb_coordinates.geometry.type_position'tag then
			return xy 
				& space & keyword_rotation & to_string (rot (et_pcb_coordinates.geometry.type_position (point)));
				-- position x 162.560 y 98.240 rotation 180.00
			
		elsif point'tag = type_package_position'tag then
			return xy
				& space & keyword_rotation & to_string (rot (et_pcb_coordinates.geometry.type_position (point)))
				& space & keyword_face & to_string (get_face (type_package_position (point)));
				-- position x 162.560 y 98.240 rotation 180.00 face top
		else
			return xy;
		end if;

	end position;
	
	procedure write_text_properties (text : in et_libraries.type_text_basic'class) is
		use et_coordinates.geometry;
	begin
		write (keyword => keyword_size, parameters => to_string (text.size));
		write (keyword => et_libraries.keyword_line_width, parameters => to_string (text.line_width));
		write (keyword => keyword_rotation, parameters => to_string (text.rotation));
		write (keyword => keyword_style, parameters => et_libraries.to_string (text.style));
		write (keyword => keyword_alignment, parameters => space &
				keyword_horizontal & et_libraries.to_string (text.alignment.horizontal) & space &
				keyword_vertical   & et_libraries.to_string (text.alignment.vertical)
				);
		--write (keyword => keyword_hidden, parameters => et_libraries.to_string (text.visible)); -- CS: no need. probably useless
	end write_text_properties;

	procedure write_text_properties (text : in et_packages.type_text'class) is
		use et_packages;
		use et_pcb_coordinates.geometry;
	begin
-- 		write (keyword => keyword_position, parameters => position (text.position) & 
-- 			space & keyword_rotation & to_string (get_angle (text.position))
-- 			  ); -- position x 0.000 y 5.555 rotation 0.00

		write (keyword => keyword_position, parameters => position (text.position));
			-- position x 0.000 y 5.555 rotation 0.00
		
		write (keyword => keyword_size, parameters => space & keyword_width & to_string (text.dimensions.width) 
		   & space & keyword_height & to_string (text.dimensions.height)); -- size width 1.000 height 1.000
		
		write (keyword => keyword_line_width, parameters => to_string (text.line_width));
		write (keyword => keyword_alignment, parameters => space &
				keyword_horizontal & et_libraries.to_string (text.alignment.horizontal) & space &
				keyword_vertical   & et_libraries.to_string (text.alignment.vertical)
				);
		-- CS write (keyword => keyword_hidden, parameters => space & to_lower (boolean'image (text.hidden)));
	end write_text_properties;

	procedure write_text_properties_with_face (
		text	: in et_packages.type_text'class;
		face	: in et_pcb_coordinates.type_face) 
		is
		use et_packages;
		use et_pcb_coordinates;
		use et_pcb_coordinates.geometry;
	begin
		write (keyword => keyword_position, parameters => position (text.position) & 
			space & keyword_face & to_string (face)); -- position x 0.000 y 5.555 rotation 0.00 face top

		-- CS this could be more elegant way. did not get it working
		-- 		write (keyword => keyword_position, parameters => 
		-- 			   position (type_position (text.position with face => face))
		-- 			  );
		
		write (keyword => keyword_size, parameters => space & keyword_width & to_string (text.dimensions.width) 
			   & space & keyword_height & to_string (text.dimensions.height)); -- size width 1.000 height 1.000
		
		write (keyword => keyword_line_width, parameters => to_string (text.line_width));
		write (keyword => keyword_alignment, parameters => space &
				keyword_horizontal & et_libraries.to_string (text.alignment.horizontal) & space &
				keyword_vertical   & et_libraries.to_string (text.alignment.vertical)
				);
		-- CS write (keyword => keyword_hidden, parameters => space & to_lower (boolean'image (text.hidden)));
	end write_text_properties_with_face;
		
	
	procedure line_begin is begin section_mark (section_line, HEADER); end;
	procedure line_end   is begin section_mark (section_line, FOOTER); end;			
	procedure arc_begin  is begin section_mark (section_arc , HEADER); end;
	procedure arc_end    is begin section_mark (section_arc , FOOTER); end;
	procedure circle_begin is begin section_mark (section_circle, HEADER); end;
	procedure circle_end   is begin section_mark (section_circle, FOOTER); end;			
	procedure fill_zone_begin is begin section_mark (section_fill_zone, HEADER); end;
	procedure fill_zone_end   is begin section_mark (section_fill_zone, FOOTER); end;
	procedure cutout_zone_begin is begin section_mark (section_cutout_zone, HEADER); end;
	procedure cutout_zone_end   is begin section_mark (section_cutout_zone, FOOTER); end;
	procedure contours_begin is begin section_mark (section_contours, HEADER); end;
	procedure contours_end   is begin section_mark (section_contours, FOOTER); end;
	procedure text_begin is begin section_mark (section_text, HEADER); end;
	procedure text_end   is begin section_mark (section_text, FOOTER); end;
	procedure placeholder_begin is begin section_mark (section_placeholder, HEADER); end;
	procedure placeholder_end   is begin section_mark (section_placeholder, FOOTER); end;
	
	procedure write_text (cursor : in et_packages.type_texts_with_content.cursor) is
		use et_packages.type_texts_with_content;
	begin
		text_begin;
		write (keyword => keyword_content, space => true, wrap => true,
			   parameters => et_libraries.to_string (element (cursor).content));
		write_text_properties (element (cursor));
		text_end;
	end write_text;

	procedure write_width (width : in et_packages.type_track_width) is 
		use et_pcb_coordinates.geometry;
	begin
		write (keyword => keyword_width, parameters => to_string (width));
	end;
	
	procedure write_line (line : in et_packages.shapes.type_line'class) is
	-- writes start and end point of a line
		use et_packages.shapes;
		use et_pcb_coordinates.geometry;		
	begin
		write (keyword => keyword_start, parameters => position (line.start_point));
		write (keyword => keyword_end  , parameters => position (line.end_point));
	end write_line;

	procedure write_arc (arc : in et_packages.shapes.type_arc'class) is 
	-- writes center, start and end point of an arc
		use et_packages.shapes;
		use et_pcb_coordinates.geometry;		
	begin
		write (keyword => keyword_center, parameters => position (arc.center));
		write (keyword => keyword_start, parameters => position (arc.start_point));
		write (keyword => keyword_end  , parameters => position (arc.end_point));
	end write_arc;

	procedure write_circle (circle : in et_packages.shapes.type_circle'class) is 
	-- writes center and radius of a circle
		use et_packages.shapes;
		use et_pcb_coordinates.geometry;		
	begin
		write (keyword => keyword_center, parameters => position (circle.center));
		write (keyword => keyword_radius, parameters => to_string (circle.radius));
	end write_circle;
		
	procedure write_polygon_segments (polygon : in et_packages.shapes.type_polygon_base) is
	-- writes the segments of a polygon (lines, arcs and circles)
		use et_packages;
		use shapes.pac_polygon_lines;
		use shapes.pac_polygon_arcs;
		use shapes.pac_polygon_circles;		
		
		procedure write_line (cursor : in shapes.pac_polygon_lines.cursor) is begin
			line_begin;
			write_line (element (cursor));
			line_end;
		end;

		procedure write_arc (cursor : in shapes.pac_polygon_arcs.cursor) is begin
			arc_begin;
			write_arc (element (cursor));
			arc_end;
		end;

		procedure write_circle (cursor : in shapes.pac_polygon_circles.cursor) is begin
			circle_begin;
			write_circle (element (cursor));
			circle_end;
		end;

	begin
		iterate (polygon.segments.lines, write_line'access);
		iterate (polygon.segments.arcs, write_arc'access);
		iterate (polygon.segments.circles, write_circle'access);		
	end write_polygon_segments;

	procedure write_hatching (hatching : in et_packages.type_hatching) is
		use et_packages;
		use et_pcb_coordinates.geometry;
	begin
		write (keyword => keyword_hatching_line_width  , parameters => to_string (hatching.line_width));
		write (keyword => keyword_hatching_line_spacing, parameters => to_string (hatching.spacing));
		-- CS border width
	end;

	procedure write_easing (easing: in et_packages.type_easing) is
		use et_pcb_coordinates.geometry;
		use et_packages;
	begin
		write (keyword => keyword_corner_easing, space => true, parameters => to_string (easing.style));
		write (keyword => keyword_easing_radius, parameters => to_string (easing.radius));
	end;

	procedure write_thermal (thermal : in et_pcb.type_thermal) is
		use et_pcb_coordinates.geometry;
		use et_pcb;
	begin
		write (keyword => keyword_pad_technology, parameters => to_string (thermal.technology));
		write (keyword => keyword_thermal_width , parameters => to_string (thermal.width));
		write (keyword => keyword_thermal_gap   , parameters => to_string (thermal.gap));	
	end;

	procedure write_width_min (width : in et_packages.type_track_width) is 
		use et_packages;
		use et_pcb_coordinates.geometry;
	begin
		write (keyword => keyword_min_width, parameters => to_string (width));
	end;

	procedure write_isolation (iso : in et_packages.type_track_clearance) is 
		use et_packages;
		use et_pcb_coordinates.geometry;
	begin
		write (keyword => keyword_isolation, parameters => to_string (iso));
	end;

	procedure write_priority (prio : in et_pcb.type_polygon_priority) is
		use et_pcb;
	begin
		write (keyword => keyword_priority , parameters => to_string (prio));
	end;

	procedure write_signal_layer (layer : in et_pcb_stack.type_signal_layer) is 
		use et_pcb_stack;
	begin
		write (keyword => keyword_layer, space => true, parameters => to_string (layer));
	end;

	procedure write_fill_stlye (fill_style : in et_packages.type_fill_style) is
		use et_packages;
	begin
		write (keyword => keyword_fill_style, space => true, parameters => to_string (fill_style));
	end;

	procedure write_fill_status (filled : in et_packages.shapes.type_filled) is
		use et_packages.shapes;
	begin
		write (keyword => keyword_filled, space => true, parameters => to_string (filled));
	end;
	
	procedure write_pad_connection (connection : in et_pcb.type_polygon_pad_connection) is
		use et_pcb;
	begin
		write (keyword => keyword_pad_connection, parameters => to_string (connection));
	end;

	procedure write_pad_technology (techno : in et_pcb.type_polygon_pad_technology) is
		use et_pcb;
	begin
		write (keyword => keyword_pad_technology, parameters => to_string (techno));
	end;	

	procedure write_signal_layers (layers : in et_pcb_stack.type_signal_layers.set) is
		use et_pcb_stack;
	begin
		write (keyword => keyword_layers, space => true, parameters => to_string (layers));
	end;
	
	procedure write_circle_fillable (circle : in et_packages.type_fillable_circle) is 
		use et_packages;
		use et_packages.shapes;
		use et_pcb_coordinates.geometry;		
	begin
		circle_begin;
		write_circle (circle);
		write (keyword => keyword_filled, parameters => latin_1.space & to_string (circle.filled));
		case circle.filled is
			when NO =>
				write (keyword => keyword_width, parameters => to_string (circle.border_width));
				
			when YES =>
				write (keyword => keyword_fill_style, parameters => latin_1.space & to_string (circle.fill_style));

				case circle.fill_style is
					when SOLID => null;
					when HATCHED =>
						write (keyword => keyword_hatching_line_width  , parameters => to_string (circle.hatching.line_width));
						write (keyword => keyword_hatching_line_spacing, parameters => to_string (circle.hatching.spacing));
				end case;

		end case;
		circle_end;
	end write_circle_fillable;

	-- CS unify the follwing two procedures write_circle_copper:
	procedure write_circle_copper (circle : in et_packages.type_copper_circle) is 
	-- Writes the properties of a circle in copper as used in a package.
		use et_packages;
		use et_packages.shapes;
		use et_pcb_coordinates.geometry;		
	begin
		circle_begin;
		write_circle (circle);
		write (keyword => keyword_filled, parameters => latin_1.space & to_string (circle.filled));
		case circle.filled is
			when NO =>
				write (keyword => keyword_width, parameters => to_string (circle.border_width));
				
			when YES =>
				write (keyword => keyword_fill_style, parameters => latin_1.space & to_string (circle.fill_style));

				case circle.fill_style is
					when SOLID => null;
					when HATCHED =>
						write (keyword => keyword_hatching_line_width  , parameters => to_string (circle.hatching.line_width));
						write (keyword => keyword_hatching_line_spacing, parameters => to_string (circle.hatching.spacing));
				end case;

		end case;
		circle_end;
	end write_circle_copper;

	procedure write_circle_copper (circle : in et_pcb.type_copper_circle) is 
	-- Writes the properties of a circle in copper as used in a freetrack.		
		use et_packages;
		use et_packages.shapes;
		use et_pcb_coordinates.geometry;		
	begin
		circle_begin;
		write_circle (circle);
		write_signal_layer (circle.layer);

		-- the signal layer:
		write (keyword => keyword_filled, parameters => latin_1.space & to_string (circle.filled));
		
		case circle.filled is
			when NO =>
				write (keyword => keyword_width, parameters => to_string (circle.border_width));
				
			when YES =>
				write (keyword => keyword_fill_style, parameters => latin_1.space & to_string (circle.fill_style));

				case circle.fill_style is
					when SOLID => null;
					when HATCHED =>
						write (keyword => keyword_hatching_line_width  , parameters => to_string (circle.hatching.line_width));
						write (keyword => keyword_hatching_line_spacing, parameters => to_string (circle.hatching.spacing));
				end case;

		end case;
		circle_end;
	end write_circle_copper;

	
-- KEEPOUT
	procedure write_line (cursor : in et_packages.type_keepout_lines.cursor) is
		use et_packages.type_keepout_lines;
		use et_pcb_coordinates.geometry;		
	begin
		line_begin;
		write_line (element (cursor));
		line_end;
	end write_line;

	procedure write_arc (cursor : in et_packages.type_keepout_arcs.cursor) is 
		use et_packages.type_keepout_arcs;
		use et_pcb_coordinates.geometry;		
	begin
		arc_begin;
		write_arc (element (cursor));
		arc_end;
	end write_arc;
	
	procedure write_circle (cursor : in et_packages.type_keepout_circles.cursor) is 
		use et_packages;
		use et_packages.shapes;
		use et_pcb_coordinates.geometry;		
		use type_keepout_circles;
	begin
		circle_begin;
		write_circle (element (cursor));
		write (keyword => keyword_filled, space => true, parameters => to_string (element (cursor).filled));
		circle_end;
	end write_circle;
	
	procedure write_polygon (cursor : in et_packages.type_keepout_polygons.cursor) is 
		use et_packages;
		use type_keepout_polygons;
	begin
		fill_zone_begin;
		write_fill_status (element (cursor).filled);
		write_polygon_segments (shapes.type_polygon_base (element (cursor)));
		fill_zone_end;
	end write_polygon;

	procedure write_cutout (cursor : in et_packages.pac_keepout_cutouts.cursor) is 
		use et_packages;
		use pac_keepout_cutouts;
	begin
		cutout_zone_begin;
		write_easing (element (cursor).easing);
		write_polygon_segments (shapes.type_polygon_base (element (cursor)));
		cutout_zone_end;
	end;

	
-- STOP MASK
	procedure write_line (cursor : in et_packages.type_stop_lines.cursor) is 
		use et_packages;
		use type_stop_lines;
		use et_pcb_coordinates.geometry;
	begin
		line_begin;
		write_line (element (cursor));
		write (keyword => keyword_width, parameters => to_string (element (cursor).width));
		line_end;
	end write_line;

	procedure write_arc (cursor : in et_packages.type_stop_arcs.cursor) is 
		use et_packages;
		use type_stop_arcs;
		use et_pcb_coordinates.geometry;
	begin
		arc_begin;
		write_arc (element (cursor));
		write (keyword => keyword_width, parameters => to_string (element (cursor).width));
		arc_end;
	end write_arc;

	procedure write_circle (cursor : in et_packages.type_stop_circles.cursor) is 
		use et_packages;
		use type_stop_circles;
	begin
		write_circle_fillable (element (cursor));
	end write_circle;
	
	procedure write_polygon (cursor : in et_packages.type_stop_polygons.cursor) is 
		use et_packages;
		use type_stop_polygons;
	begin
		fill_zone_begin;
		write_easing (element (cursor).easing);
		write_fill_stlye (element (cursor).fill_style);
					  
		if element (cursor).fill_style = HATCHED then
			write_hatching (element (cursor).hatching);
		end if;

		write_polygon_segments (shapes.type_polygon_base (element (cursor)));
		fill_zone_end;
	end write_polygon;

	procedure write_cutout (cursor : in et_packages.pac_stop_cutouts.cursor) is 
		use et_packages;
		use pac_stop_cutouts;
	begin
		cutout_zone_begin;
		write_easing (element (cursor).easing);
		write_polygon_segments (shapes.type_polygon_base (element (cursor)));
		cutout_zone_end;
	end;
	
-- STENCIL (OR SOLDER PASTE MASK)
	procedure write_line (cursor : in et_packages.type_stencil_lines.cursor) is 
		use et_packages;
		use type_stencil_lines;
		use et_pcb_coordinates.geometry;
	begin
		line_begin;
		write_line (element (cursor));
		write (keyword => keyword_width, parameters => to_string (element (cursor).width));
		line_end;
	end write_line;

	procedure write_arc (cursor : in et_packages.type_stencil_arcs.cursor) is 
		use et_packages;
		use type_stencil_arcs;
		use et_pcb_coordinates.geometry;
	begin
		arc_begin;
		write_arc (element (cursor));
		write (keyword => keyword_width, parameters => to_string (element (cursor).width));
		arc_end;
	end write_arc;

	procedure write_circle (cursor : in et_packages.type_stencil_circles.cursor) is 
		use et_packages;
		use type_stencil_circles;
	begin
		write_circle_fillable (element (cursor));
	end write_circle;
	
	procedure write_polygon (cursor : in et_packages.type_stencil_polygons.cursor) is 
		use et_packages;
		use type_stencil_polygons;
	begin
		fill_zone_begin;
		write_easing (element (cursor).easing);
		write_fill_stlye (element (cursor).fill_style);
					  
		if element (cursor).fill_style = HATCHED then
			write_hatching (element (cursor).hatching);
		end if;

		write_polygon_segments (shapes.type_polygon_base (element (cursor)));
		fill_zone_end;
	end write_polygon;

	procedure write_cutout (cursor : in et_packages.pac_stencil_cutouts.cursor) is 
		use et_packages;
		use pac_stencil_cutouts;
	begin
		cutout_zone_begin;
		write_easing (element (cursor).easing);
		write_polygon_segments (shapes.type_polygon_base (element (cursor)));
		cutout_zone_end;
	end;
	
-- ROUTE RESTRICT
	procedure write_line (cursor : in et_packages.type_route_restrict_lines.cursor) is 
		use et_packages;
		use et_pcb_stack;
		use type_route_restrict_lines;
		use et_pcb_coordinates.geometry;		
	begin
		line_begin;
		write_line (element (cursor));
		write_signal_layers (element (cursor).layers);
		line_end;
	end write_line;

	procedure write_arc (cursor : in et_packages.type_route_restrict_arcs.cursor) is 
		use et_packages;
		use et_pcb_stack;
		use type_route_restrict_arcs;
		use et_pcb_coordinates.geometry;		
	begin
		arc_begin;
		write_arc (element (cursor));		
		write_signal_layers (element (cursor).layers);
		arc_end;
	end write_arc;

	procedure write_circle (cursor : in et_packages.type_route_restrict_circles.cursor) is 
		use et_packages;
		use type_route_restrict_circles;
		use et_pcb_coordinates.geometry;		
	begin
		circle_begin;
		write_circle (element (cursor));
		write_fill_status (element (cursor).filled);
		write_signal_layers (element (cursor).layers);
		circle_end;
	end write_circle;
	
	procedure write_polygon (cursor : in et_packages.type_route_restrict_polygons.cursor) is 
		use et_packages;
		use type_route_restrict_polygons;
	begin
		fill_zone_begin;
		write_fill_status (element (cursor).filled);
		write_signal_layers (element (cursor).layers);
		write_polygon_segments (shapes.type_polygon_base (element (cursor)));
		fill_zone_end;
	end write_polygon;

	procedure write_cutout (cursor : in et_packages.pac_route_restrict_cutouts.cursor) is 
		use et_packages;
		use pac_route_restrict_cutouts;
	begin
		cutout_zone_begin;
		write_signal_layers (element (cursor).layers);
		write_easing (element (cursor).easing);
		write_polygon_segments (shapes.type_polygon_base (element (cursor)));
		cutout_zone_end;
	end;

	
-- VIA RESTRICT
	procedure write_line (cursor : in et_packages.type_via_restrict_lines.cursor) is 
		use et_packages;
		use et_pcb_stack;
		use type_via_restrict_lines;
		use et_pcb_coordinates.geometry;		
	begin
		line_begin;
		write_line (element (cursor));		
		write_signal_layers (element (cursor).layers);
		line_end;
	end write_line;

	procedure write_arc (cursor : in et_packages.type_via_restrict_arcs.cursor) is 
		use et_packages;
		use et_pcb_stack;
		use type_via_restrict_arcs;
		use et_pcb_coordinates.geometry;		
	begin
		arc_begin;
		write_arc (element (cursor));
		write_signal_layers (element (cursor).layers);
		arc_end;
	end write_arc;

	procedure write_circle (cursor : in et_packages.type_via_restrict_circles.cursor) is 
		use et_packages;
		use et_packages.shapes;		
		use et_pcb_stack;		
		use type_via_restrict_circles;
		use et_pcb_coordinates.geometry;		
	begin
		circle_begin;
		write_circle (element (cursor));
		write_fill_status (element (cursor).filled);
		write_signal_layers (element (cursor).layers);
		circle_end;
	end write_circle;
	
	procedure write_polygon (cursor : in et_packages.type_via_restrict_polygons.cursor) is 
		use et_packages;
		use et_pcb_stack;
		use type_via_restrict_polygons;
	begin
		fill_zone_begin;
		write_fill_status (element (cursor).filled);
		write_signal_layers (element (cursor).layers);			
		write_polygon_segments (shapes.type_polygon_base (element (cursor)));
		fill_zone_end;
	end write_polygon;

	procedure write_cutout (cursor : in et_packages.pac_via_restrict_cutouts.cursor) is 
		use et_packages;
		use pac_via_restrict_cutouts;
	begin
		cutout_zone_begin;
		write_signal_layers (element (cursor).layers);
		write_easing (element (cursor).easing);
		write_polygon_segments (shapes.type_polygon_base (element (cursor)));
		cutout_zone_end;
	end;
	
-- BOARD CONTOUR
	procedure write_line (cursor : in et_packages.type_pcb_contour_lines.cursor) is 
		use et_packages;
		use type_pcb_contour_lines;
		use et_pcb_coordinates.geometry;		
	begin
		line_begin;
		write_line (element (cursor));
		line_end;
	end write_line;

	procedure write_arc (cursor : in et_packages.type_pcb_contour_arcs.cursor) is 
		use et_packages;
		use type_pcb_contour_arcs;
		use et_pcb_coordinates.geometry;		
	begin
		arc_begin;
		write_arc (element (cursor));
		arc_end;
	end write_arc;

	procedure write_circle (cursor : in et_packages.type_pcb_contour_circles.cursor) is 
		use et_packages;
		use type_pcb_contour_circles;
		use et_pcb_coordinates.geometry;		
	begin
		circle_begin;
		write_circle (element (cursor));
		circle_end;
	end write_circle;
	
	procedure write_line (cursor : in et_pcb.type_pcb_contour_lines.cursor) is 
		use et_pcb;
		use type_pcb_contour_lines;
		use et_pcb_coordinates.geometry;		
	begin
		line_begin;
		write_line (element (cursor));
		write (keyword => keyword_locked, space => true, parameters => to_string (element (cursor).locked));
		line_end;
	end write_line;
	
	procedure write_arc (cursor : in et_pcb.type_pcb_contour_arcs.cursor) is 
		use et_pcb;
		use type_pcb_contour_arcs;
		use et_pcb_coordinates.geometry;		
	begin
		arc_begin;
		write_arc (element (cursor));
		write (keyword => keyword_locked, parameters => to_string (element (cursor).locked));
		arc_end;
	end write_arc;

	procedure write_circle (cursor : in et_pcb.type_pcb_contour_circles.cursor) is 
		use et_pcb;
		use type_pcb_contour_circles;
		use et_pcb_coordinates.geometry;		
	begin
		circle_begin;
		write_circle (element (cursor));
		write (keyword => keyword_locked, parameters => to_string (element (cursor).locked));
		circle_end;
	end write_circle;

-- SILK SCREEN
	procedure write_line (cursor : in et_packages.type_silk_lines.cursor) is 
		use et_packages;
		use type_silk_lines;
		use et_pcb_coordinates.geometry;		
	begin
		line_begin;
		write_line (element (cursor));		
		write (keyword => keyword_width, parameters => to_string (element (cursor).width));
		line_end;
	end write_line;

	procedure write_arc (cursor : in et_packages.type_silk_arcs.cursor) is 
		use et_packages;
		use type_silk_arcs;
		use et_pcb_coordinates.geometry;		
	begin
		arc_begin;
		write_arc (element (cursor));
		write (keyword => keyword_width, parameters => to_string (element (cursor).width));
		arc_end;
	end write_arc;

	procedure write_circle (cursor : in et_packages.type_silk_circles.cursor) is 
		use et_packages;
		use type_silk_circles;
	begin
		write_circle_fillable (element (cursor));
	end write_circle;
	
	procedure write_polygon (cursor : in et_packages.pac_silk_polygons.cursor) is 
		use et_packages;
		use pac_silk_polygons;
	begin
		fill_zone_begin;

		write_easing (element (cursor).easing);
		write_fill_stlye (element (cursor).fill_style);

		case element (cursor).fill_style is
			when HATCHED =>
				write_hatching (element (cursor).hatching);

			when others => null;
		end case;
			
		write_polygon_segments (shapes.type_polygon_base (element (cursor)));

		fill_zone_end;

	end write_polygon;

	procedure write_cutout (cursor : in et_packages.pac_silk_cutouts.cursor) is 
		use et_packages;
		use pac_silk_cutouts;
	begin
		cutout_zone_begin;
		write_easing (element (cursor).easing);
		write_polygon_segments (shapes.type_polygon_base (element (cursor)));
		cutout_zone_end;
	end;
	
-- ASSEMBLY DOCUMENTATION
	procedure write_line (cursor : in et_packages.type_doc_lines.cursor) is 
		use et_packages;
		use type_doc_lines;
		use et_pcb_coordinates.geometry;		
	begin
		line_begin;
		write_line (element (cursor));		
		write (keyword => keyword_width, parameters => to_string (element (cursor).width));
		line_end;
	end write_line;

	procedure write_arc (cursor : in et_packages.type_doc_arcs.cursor) is 
		use et_packages;
		use type_doc_arcs;
		use et_pcb_coordinates.geometry;		
	begin
		arc_begin;
		write_arc (element (cursor));
		write (keyword => keyword_width, parameters => to_string (element (cursor).width));
		arc_end;
	end write_arc;

	procedure write_circle (cursor : in et_packages.type_doc_circles.cursor) is
		use et_packages;
		use type_doc_circles;
	begin
		write_circle_fillable (element (cursor));
	end write_circle;
	
	procedure write_polygon (cursor : in et_packages.pac_doc_polygons.cursor) is 
		use et_packages;
		use pac_doc_polygons;
	begin
		fill_zone_begin;

		write_easing (element (cursor).easing);
		write_fill_stlye (element (cursor).fill_style);

		case element (cursor).fill_style is
			when HATCHED =>
				write_hatching (element (cursor).hatching);

			when others => null;
		end case;
			
		write_polygon_segments (shapes.type_polygon_base (element (cursor)));

		fill_zone_end;
	end write_polygon;

	procedure write_cutout (cursor : in et_packages.pac_doc_cutouts.cursor) is 
		use et_packages;
		use pac_doc_cutouts;
	begin
		cutout_zone_begin;
		write_easing (element (cursor).easing);
		write_polygon_segments (shapes.type_polygon_base (element (cursor)));
		cutout_zone_end;
	end;
	
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
			write (keyword => keyword_generic_name, space => true, parameters => to_string (element (instance_cursor).generic_name));
			write (keyword => keyword_instance_name, space => true, parameters => et_general.to_string (key (instance_cursor)));
			section_mark (section_module, FOOTER);
		end;

		procedure query_connections (connection_cursor : in type_module_connectors.cursor) is
			con : type_connector := element (connection_cursor);
		begin
			section_mark (section_connector, HEADER);
			write (keyword => keyword_instance_A, space => true, parameters => to_string (con.instance_A));
			write (keyword => keyword_purpose_A, space => true, wrap => true, parameters => et_libraries.to_string (con.purpose_A));
			new_line;
			write (keyword => keyword_instance_B, space => true, parameters => to_string (con.instance_B));
			write (keyword => keyword_purpose_B, space => true, wrap => true, parameters => et_libraries.to_string (con.purpose_B));

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
		
		procedure query_net_classes is
			use et_pcb;
			use et_pcb.type_net_classes;
			use et_pcb_coordinates.geometry;

			procedure write (class_cursor : in type_net_classes.cursor) is begin
				log (text => "net class " & to_string (key (class_cursor)), level => log_threshold + 1);
				section_mark (section_net_class, HEADER);

				write (keyword => keyword_name, space => true, parameters => to_string (key (class_cursor)));
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
			write (keyword => keyword_default, space => true, parameters => 
				   keyword_pos_x & to_string (element (module_cursor).grid.x) & space &
				   keyword_pos_y & to_string (element (module_cursor).grid.y));
			section_mark (section_schematic, FOOTER);

			section_mark (section_board, HEADER);
			write (keyword => keyword_default, space => true, parameters => 
				   keyword_pos_x & to_string (element (module_cursor).board.grid.x) & space &
				   keyword_pos_y & to_string (element (module_cursor).board.grid.y));
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
			write (keyword => keyword_conductor, space => true,
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
						use et_libraries;
					begin -- query_labels
						if not is_empty (segment.labels) then
							section_mark (section_labels, HEADER);
							while label_cursor /= type_net_labels.no_element loop
								section_mark (section_label, HEADER);
								
								write (keyword => keyword_position, parameters => position (element (label_cursor).position));
								write (keyword => keyword_rotation, parameters => to_string (element (label_cursor).rotation));
								write (keyword => keyword_size, parameters => to_string (element (label_cursor).size));
								write (keyword => keyword_style, parameters => to_string (element (label_cursor).style));
								write (keyword => keyword_line_width, parameters => to_string (element (label_cursor).width));

								write (keyword => keyword_appearance, parameters =>
									et_schematic.to_string (appearance => element (label_cursor).appearance));
								
								-- a tag label also indicates a signal direction
								if element (label_cursor).appearance = TAG then
									write (keyword => keyword_direction, parameters => to_string (element (label_cursor).direction));
								end if;
								
								section_mark (section_label, FOOTER);
								next (label_cursor);
							end loop;
							section_mark (section_labels, FOOTER);
						end if;
					end query_labels;

					procedure query_junctions (segment : in type_net_segment) is
					begin
						if segment.junctions.start_point then
							write (keyword => keyword_junction, space => true, parameters => keyword_start);
						end if;

						if segment.junctions.end_point then
							write (keyword => keyword_junction, space => true, parameters => keyword_end);
						end if;
					end query_junctions;
					
					procedure query_device_ports (segment : in type_net_segment) is
						use et_libraries;
						port_cursor : type_ports_device.cursor := segment.ports_devices.first;
					begin -- query_device_ports
						while port_cursor /= type_ports_device.no_element loop
							write (keyword => keyword_device, parameters => 
								space & et_libraries.to_string (element (port_cursor).device_name)
								& space & keyword_port & space
								& et_libraries.to_string (element (port_cursor).port_name)
								); -- device IC1 port A
							next (port_cursor);
						end loop;
					end query_device_ports;

					procedure query_submodule_ports (segment : in type_net_segment) is
						use et_libraries;
						port_cursor : type_ports_submodule.cursor := segment.ports_submodules.first;
					begin -- query_submodule_ports
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
						use et_libraries;
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

					write (keyword => keyword_position, parameters => position (element (strand_cursor).position));

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
				use et_packages.shapes;
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
					write (keyword => keyword_layer, space => true, parameters => to_string (element (line_cursor).layer));
					write (keyword => keyword_width, parameters => to_string (element (line_cursor).width));

					section_mark (section_line, FOOTER);
					next (line_cursor);
				end loop;

				while arc_cursor /= pac_copper_arcs.no_element loop
					section_mark (section_arc, HEADER);

					write (keyword => keyword_center, parameters => position (element (arc_cursor).center));
					write (keyword => keyword_start , parameters => position (element (arc_cursor).start_point));
					write (keyword => keyword_end   , parameters => position (element (arc_cursor).end_point));
					write (keyword => keyword_width , parameters => to_string (element (arc_cursor).width));
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
						when THERMAL => 
							write_pad_connection (element (polygon_solid_cursor).connection);
							write_thermal (element (polygon_solid_cursor).thermal);
			
						when SOLID =>
							write_pad_technology (element (polygon_solid_cursor).technology);
							
					end case;

					write_polygon_segments (shapes.type_polygon_base (element (polygon_solid_cursor)));
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
						when THERMAL => 
							write_pad_connection (element (polygon_hatched_cursor).connection);
							write_thermal (element (polygon_hatched_cursor).thermal);
			
						when SOLID =>
							write_pad_technology (element (polygon_hatched_cursor).technology);

					end case;

					write_polygon_segments (shapes.type_polygon_base (element (polygon_hatched_cursor)));
					fill_zone_end;
					next (polygon_hatched_cursor);
				end loop;

				-- cutout zones
				while cutout_zone_cursor /= et_pcb.pac_copper_cutouts.no_element loop
					cutout_zone_begin;
					write_signal_layer (element (cutout_zone_cursor).layer);
					write_easing (element (cutout_zone_cursor).easing);
					write_polygon_segments (shapes.type_polygon_base (element (cutout_zone_cursor)));
					cutout_zone_end;
					next (cutout_zone_cursor);
				end loop;
				
				section_mark (section_route, FOOTER);
			end query_route;

			procedure write (net_cursor : in et_schematic.type_nets.cursor) is begin
				log (text => "net " & et_general.to_string (key (net_cursor)), level => log_threshold + 1);
				section_mark (section_net, HEADER);

				write (keyword => keyword_name, parameters => et_general.to_string (key (net_cursor)), space => true);
				write (keyword => keyword_class, parameters => to_string (element (net_cursor).class), space => true);
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
			use type_devices;

			procedure query_units (device_name : in et_libraries.type_device_name; device : in et_schematic.type_device) is
				use et_schematic.type_units;
				unit_cursor : type_units.cursor := device.units.first;

				use et_coordinates.geometry;
				
				procedure write_placeholder (ph : in et_libraries.type_text_placeholder) is
				begin
					section_mark (section_placeholder, HEADER);
					write (keyword => keyword_meaning, parameters => et_libraries.to_string (ph.meaning));
					write (keyword => keyword_position, parameters => position (ph.position));
					write_text_properties (ph);
					section_mark (section_placeholder, FOOTER);
				end write_placeholder;

				use et_libraries;
				
			begin -- query_units
				section_mark (section_units, HEADER);
				while unit_cursor /= type_units.no_element loop
					section_mark (section_unit, HEADER);
					write (keyword => keyword_name, parameters => et_libraries.to_string (key (unit_cursor)), space => true);
					write (keyword => keyword_position, parameters => position (element (unit_cursor).position)); -- position sheet 1 x 147.32 y 96.97
					--write (keyword => keyword_rotation, parameters => to_string (element (unit_cursor).rotation)); -- rotation 180.0
					write (keyword => keyword_rotation, parameters => to_string (rot (element (unit_cursor).position))); -- rotation 180.0
					write (keyword => keyword_mirrored, parameters => to_string (element (unit_cursor).mirror, verbose => false)); -- x_axis, y_axis, none

					if element (unit_cursor).appearance = et_libraries.SCH_PCB then
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
				device_name : in et_libraries.type_device_name;
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

			procedure write (device_cursor : in type_devices.cursor) is begin
				section_mark (section_device, HEADER);
				write (keyword => keyword_name, parameters => et_libraries.to_string (key (device_cursor)), space => true);
				write (keyword => keyword_appearance, parameters => et_libraries.to_string (element (device_cursor).appearance));
				write (keyword => keyword_model, parameters => et_libraries.to_string (element (device_cursor).model), space => true);

				case element (device_cursor).appearance is
					when et_libraries.SCH_PCB =>
						write (keyword => keyword_value   , parameters => et_libraries.to_string (element (device_cursor).value), space => true);
						write (keyword => keyword_variant , parameters => et_libraries.to_string (element (device_cursor).variant), space => true);
						write (keyword => keyword_partcode, parameters => material.to_string (element (device_cursor).partcode), space => true);
						write (keyword => keyword_purpose , parameters => et_libraries.to_string (element (device_cursor).purpose), space => true, wrap => true);
						
						section_mark (section_package, HEADER);

						-- This is the position of the package in the layout, 
						write (keyword => keyword_position, parameters => -- position x 34.5 y 60.1 face top/bottom
							   position (element (device_cursor).position));

						-- Flip status:
						write (keyword => keyword_flipped, parameters => et_pcb.to_string (element (device_cursor).flipped), space => true);
					
						query_element (device_cursor, query_placeholders'access);
						section_mark (section_package, FOOTER);
						
					when et_libraries.SCH => null;
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
			use type_variants;

			procedure query_devices (
				variant_name	: in type_variant_name.bounded_string;
				variant			: in type_variant) is
				use assembly_variants.type_devices;
				device_cursor : assembly_variants.type_devices.cursor := variant.devices.first;

				function purpose return string is 
					use et_libraries;
					use type_device_purpose;
				begin
					if length (element (device_cursor).purpose) > 0 then
						return latin_1.space & keyword_purpose & latin_1.space &
							enclose_in_quotes (
								text_in => et_libraries.to_string (element (device_cursor).purpose),
								quote	=> latin_1.quotation);
					else
						return "";
					end if;
				end;

				use et_libraries;
				
			begin -- query_devices
				while device_cursor /= assembly_variants.type_devices.no_element loop
					case element (device_cursor).mounted is
						when NO =>
							write (
								keyword		=> keyword_device,
								parameters	=> et_libraries.to_string (key (device_cursor)) & 
												latin_1.space & keyword_not_mounted,
								space 		=> true);

						when YES =>
							write (
								keyword		=> keyword_device,
								parameters	=> et_libraries.to_string (key (device_cursor)) & 
									latin_1.space &
									keyword_value & latin_1.space &
									et_libraries.to_string (element (device_cursor).value) &
									latin_1.space & keyword_partcode & latin_1.space &
									material.to_string (element (device_cursor).partcode) &
									purpose,
								space 		=> true);

					end case;
					
					next (device_cursor);
				end loop;
			end query_devices;

			procedure query_submodules (
				variant_name	: in type_variant_name.bounded_string;
				variant			: in type_variant) is
				use assembly_variants;
				use assembly_variants.type_submodules;
				submodule_cursor : assembly_variants.type_submodules.cursor := variant.submodules.first;
			begin
				while submodule_cursor /= type_submodules.no_element loop
					write (
						keyword		=> keyword_submodule,
						parameters	=> et_general.to_string (key (submodule_cursor)) &
										latin_1.space & keyword_variant & latin_1.space &
										to_variant (element (submodule_cursor).variant),
						space		=> true);
					
					next (submodule_cursor);
				end loop;
			end query_submodules;

			procedure write (variant_cursor : in type_variants.cursor) is begin
				section_mark (section_assembly_variant, HEADER);
				write (keyword => keyword_name, parameters => to_variant (key (variant_cursor)), space => true);
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
		
		procedure query_frames is		
			use et_libraries;
			use type_frame_template_name;
		begin
			section_mark (section_drawing_frames, HEADER);
			section_mark (section_schematic, HEADER);			
			write (
				keyword 	=> keyword_template, 
				space 		=> true, 
				parameters	=> et_libraries.to_string (element (module_cursor).frame_template_schematic));
			
			-- CS frame count ?
			-- CS description ?
			
			section_mark (section_schematic, FOOTER);			

			section_mark (section_board, HEADER);			
			write (
				keyword		=> keyword_template, 
				space 		=> true, 
				parameters	=> et_libraries.to_string (element (module_cursor).frame_template_board));
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
				write (keyword => keyword_name, space => true, parameters => et_general.to_string (key (port_cursor))); -- name clk_out
				write (keyword => keyword_position, parameters => position (element (port_cursor).position)); -- position x 0 y 10
				write (keyword => keyword_direction, parameters => to_string (element (port_cursor).direction)); -- direction master/slave
				section_mark (section_port, FOOTER);
			end;

			procedure write (submodule_cursor : in type_submodules.cursor) is 
				use et_coordinates.geometry;
			begin
				section_mark (section_submodule, HEADER);
				write (keyword => keyword_name, space => true, parameters => et_general.to_string (key (submodule_cursor))); -- name stepper_driver_1
				write (keyword => keyword_file, space => true, parameters => type_submodule_path.to_string (element (submodule_cursor).file)); -- file $ET_TEMPLATES/motor_driver.mod

				write (keyword => keyword_position, parameters => position (element (submodule_cursor).position));
				write (keyword => keyword_size, parameters => 
					space & keyword_pos_x & to_string (element (submodule_cursor).size.x) &
					space & keyword_pos_y & to_string (element (submodule_cursor).size.y)); -- size x 50 y 70
				
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
				write (keyword => keyword_content, space => true, wrap => true,
					   parameters => et_libraries.to_string (element (text_cursor).content));
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

				write_polygon_segments (shapes.type_polygon_base (element (cursor)));

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

				write_polygon_segments (shapes.type_polygon_base (element (cursor)));

				fill_zone_end;
			end;

			-- cutout zones in any signal layers
			use et_pcb.pac_copper_cutouts;
			procedure write_cutout (cursor : in et_pcb.pac_copper_cutouts.cursor) is begin
				cutout_zone_begin;
				write_signal_layer (element (cursor).layer);
				write_easing (element (cursor).easing);
				write_polygon_segments (shapes.type_polygon_base (element (cursor)));
				cutout_zone_end;
			end;

			-- texts any signal layers
			procedure write_text (cursor : in pac_texts.cursor) is begin
				text_begin;
				write (keyword => keyword_content, parameters => et_libraries.to_string (element (cursor).content));
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

	procedure write_symbol (
		symbol			: in et_libraries.type_symbol;
		log_threshold	: in et_string_processing.type_log_level) is
		use et_libraries;
		use et_libraries.shapes;
		use et_coordinates.geometry;
		use type_lines;
		use type_arcs;
		use type_circles;
		use type_symbol_texts;
		use type_ports;

		procedure write_line (cursor : in type_lines.cursor) is begin
			section_mark (section_line, HEADER);
			write (keyword => keyword_start, parameters => position (element (cursor).start_point));
			write (keyword => keyword_end  , parameters => position (element (cursor).end_point));
			write (keyword => keyword_width, parameters => to_string (element (cursor).width));
			section_mark (section_line, FOOTER);
		end write_line;

		procedure write_arc (cursor : in type_arcs.cursor) is begin
			section_mark (section_arc, HEADER);
			write (keyword => keyword_center, parameters => position (element (cursor).center));
			write (keyword => keyword_start , parameters => position (element (cursor).start_point));
			write (keyword => keyword_end   , parameters => position (element (cursor).end_point));
			write (keyword => keyword_radius, parameters => geometry.to_string (element (cursor).radius));			
			write (keyword => keyword_width , parameters => geometry.to_string (element (cursor).width));
			section_mark (section_arc, FOOTER);
		end write_arc;

		procedure write_circle (cursor : in type_circles.cursor) is begin
			section_mark (section_circle, HEADER);
			write (keyword => keyword_center, parameters => position (element (cursor).center));
			write (keyword => keyword_radius, parameters => geometry.to_string (element (cursor).radius));
			write (keyword => keyword_width , parameters => geometry.to_string (element (cursor).width));
			write (keyword => keyword_filled, parameters => to_string (element (cursor).filled));
			section_mark (section_circle, FOOTER);
		end write_circle;

		procedure write_text (cursor : in type_symbol_texts.cursor) is begin
			section_mark (section_text, HEADER);
			write (keyword => keyword_position, parameters => position (element (cursor).position));
			write (keyword => keyword_content , parameters => to_string (element (cursor).content));			
			write_text_properties (element (cursor));
			section_mark (section_text, FOOTER);
		end write_text;

		procedure write_placeholders is begin
			case symbol.appearance is
				when et_libraries.SCH_PCB =>

					section_mark (section_placeholders, HEADER);
					
					section_mark (section_placeholder, HEADER);
					write (keyword => keyword_meaning , parameters => to_string (symbol.name.meaning));
					write (keyword => keyword_position, parameters => position (symbol.name.position));
					write_text_properties (symbol.name);
					section_mark (section_placeholder, FOOTER);

					section_mark (section_placeholder, HEADER);
					write (keyword => keyword_meaning , parameters => to_string (symbol.value.meaning));
					write (keyword => keyword_position, parameters => position (symbol.value.position));
					write_text_properties (symbol.value);
					section_mark (section_placeholder, FOOTER);

					section_mark (section_placeholder, HEADER);
					write (keyword => keyword_meaning , parameters => to_string (symbol.purpose.meaning));
					write (keyword => keyword_position, parameters => position (symbol.purpose.position));
					write_text_properties (symbol.purpose);
					section_mark (section_placeholder, FOOTER);

					section_mark (section_placeholders, FOOTER);
					
				when others => null;
			end case;
		end write_placeholders;

		procedure write_port (cursor : in type_ports.cursor) is begin
			section_mark (section_port, HEADER);
			write (keyword => keyword_name, space => true, parameters => to_string (key (cursor)));
			write (keyword => keyword_position, parameters => position (element (cursor).position));
			write (keyword => keyword_direction, parameters => to_string (element (cursor).direction));
			
			case element (cursor).direction is
				when INPUT_DIGITAL =>
					write (keyword => keyword_sensitivity_edge, parameters => to_string (element (cursor).sensitivity_edge));
					write (keyword => keyword_sensitivity_level, parameters => to_string (element (cursor).sensitivity_level));

				when OUTPUT_ANALOG =>
					write (keyword => keyword_tristate, parameters => to_string (element (cursor).output_analog_tristate));
					write (keyword => keyword_weakness, parameters => to_string (element (cursor).output_analog_weakness));
					
				when OUTPUT_DIGITAL =>
					write (keyword => keyword_inverted, parameters => to_string (element (cursor).output_digital_inverted));
					write (keyword => keyword_tristate, parameters => to_string (element (cursor).output_digital_tristate));
					write (keyword => keyword_weakness, parameters => to_string (element (cursor).output_digital_weakness));
					
				when BIDIR_DIGITAL =>
					write (keyword => keyword_output_inverted, parameters => to_string (element (cursor).output_inverted));
					write (keyword => keyword_output_tristate, parameters => to_string (element (cursor).output_tristate));
					write (keyword => keyword_output_weakness, parameters => to_string (element (cursor).output_weakness));

					write (keyword => keyword_input_sensitivity_edge, parameters => to_string (element (cursor).input_sensitivity_edge));
					write (keyword => keyword_input_sensitivity_level, parameters => to_string (element (cursor).input_sensitivity_level));

				when POWER_OUT | POWER_IN =>
					write (keyword => keyword_level, parameters => to_string (element (cursor).level));
					
				when others => null; -- PASSIVE, INPUT_ANALOG, NOT_CONNECTED
			end case;
			
			write (keyword => keyword_length, parameters => geometry.to_string (element (cursor).length));
			write (keyword => keyword_rotation, parameters => to_string (element (cursor).rotation));
			write (keyword => keyword_port_name_visible, parameters => to_string (element (cursor).port_name_visible));
			write (keyword => keyword_port_name_size, parameters => geometry.to_string (element (cursor).port_name_size));
			write (keyword => keyword_terminal_name_visible, parameters => to_string (element (cursor).terminal_name_visible));
			write (keyword => keyword_terminal_name_size, parameters => geometry.to_string (element (cursor).terminal_name_size));
			section_mark (section_port, FOOTER);			
		end write_port;
		
	begin -- write_symbol
		-- appearance
		write (keyword => keyword_appearance, parameters => et_libraries.to_string (symbol.appearance));
		
		-- draw section begin
		section_mark (section_draw, HEADER);
		
		-- lines
		iterate (symbol.shapes.lines, write_line'access);

		-- arcs
		iterate (symbol.shapes.arcs, write_arc'access);
		
		-- circles
		iterate (symbol.shapes.circles, write_circle'access);

		-- draw section end
		section_mark (section_draw, FOOTER);
		
		
		-- TEXTS
		section_mark (section_texts, HEADER);
		iterate (symbol.texts, write_text'access);
		section_mark (section_texts, FOOTER);

		-- PLACEHOLDERS
		if symbol.appearance = SCH_PCB then
			write_placeholders;
		end if;

		-- PORTS
		section_mark (section_ports, HEADER);
		iterate (symbol.ports, write_port'access);
		section_mark (section_ports, FOOTER);
	end write_symbol;


	procedure expect_field_count (
		line			: in et_string_processing.type_fields_of_line;	-- the list of fields of the line
		count_expected	: in count_type;			-- the min. number of fields to expect
		warn			: in boolean := true) 		-- warn if too many fields
		is 
		use et_string_processing;
		count_found : constant count_type := field_count (line);

		function f (line : in type_fields_of_line; position : in positive) return string 
			renames et_string_processing.field;
		
		f1 : string := f (line, 1); -- CS: line must have at least one field otherwise exception occurs here
	begin
		if count_found = count_expected then null; -- fine, field count as expected
		
		elsif count_found < count_expected then -- less fields than expected
			log (ERROR, "missing parameter for '" & f1 & "' !", console => true);
			raise constraint_error;
			
		elsif count_found > count_expected then -- more fields than expeced
			if warn then
				log (WARNING, affected_line (line) & "excessive parameters after '" &
					f (line, positive (count_expected)) & "' ignored !");
			end if;
		end if;
		
	end expect_field_count;
	
	procedure save_device (
	-- Saves the given device model in a file specified by name.
		name			: in string; -- libraries/devices/resistor.dev
		device			: in et_libraries.type_device; -- the actual device model
		log_threshold	: in et_string_processing.type_log_level) is
		use et_string_processing;
		use et_libraries;
		file_handle : ada.text_io.file_type;

		use type_component_variants;
		variant_cursor : type_component_variants.cursor;
		
		procedure write_variant (
			packge	: in type_component_variant_name.bounded_string;
			variant	: in type_component_variant) is
			use type_terminal_port_map;	

			procedure write_terminal (terminal_cursor : in type_terminal_port_map.cursor) is begin
				write (keyword => keyword_terminal, parameters => 
					space & to_string (key (terminal_cursor)) & space -- terminal name like G14 or 16
					& keyword_unit & space & to_string (element (terminal_cursor).unit) -- unit name like A,B or GPIO_BANK_1
					& space & keyword_port & space & to_string (element (terminal_cursor).name) 	-- port name like CE, WE, GND
					);
			end write_terminal;

		begin -- write_variant
			write (keyword => keyword_package_model, space => true, parameters => to_string (variant.package_model)); -- CS path correct ??
			section_mark (section_terminal_port_map, HEADER);
			iterate (variant.terminal_port_map, write_terminal'access);
			section_mark (section_terminal_port_map, FOOTER);						
		end write_variant;

		use type_units_internal;
		unit_internal_cursor : type_units_internal.cursor := device.units_internal.first;
		
		use type_units_external;
		unit_external_cursor : type_units_external.cursor := device.units_external.first;

		procedure query_internal_unit (
			name	: in type_unit_name.bounded_string;
			unit	: in type_unit_internal) is
		begin -- query_internal_unit
			write (keyword => keyword_name, space => true, parameters => to_string (name));
			write (keyword => keyword_position, parameters => position (unit.position));
			write (keyword => keyword_swap_level, parameters => to_string (unit.swap_level));
			write (keyword => keyword_add_level , parameters => to_string (unit.add_level));
			section_mark (section_symbol, HEADER);
			write_symbol (unit.symbol, log_threshold + 1);
			section_mark (section_symbol, FOOTER);
		end query_internal_unit;

		procedure query_external_unit (
			name	: in type_unit_name.bounded_string;
			unit	: in type_unit_external) is
		begin -- query_external_unit
			write (keyword => keyword_name, space => true, parameters => to_string (name));
			write (keyword => keyword_position, parameters => position (unit.position));
			write (keyword => keyword_swap_level, parameters => to_string (unit.swap_level));
			write (keyword => keyword_add_level , parameters => to_string (unit.add_level));
			write (keyword => keyword_file, space => true, parameters => to_string (unit.file));
		end query_external_unit;
		
	begin -- save_device
		log (text => name, level => log_threshold);

		create (
			file 	=> file_handle,
			mode	=> out_file,
			name	=> name);

		set_output (file_handle);
		
		-- write a nice header
		put_line (comment_mark & " " & et_general.system_name & " device");
		put_line (comment_mark & " " & date);
		put_line (comment_mark & " " & row_separator_double);
		new_line;

		reset_tab_depth;

		-- prefix, appearance ...
		write (keyword => keyword_prefix, space => true, parameters => to_string (device.prefix));
		write (keyword => keyword_appearance, parameters => to_string (device.appearance));

		-- package variants
		case device.appearance is
			when SCH_PCB =>
				write (keyword => keyword_value, space => true, parameters => to_string (device.value));
				--write (keyword => keyword_partcode, space => true, parameters => to_string (device.partcode));

				section_mark (section_variants, HEADER);

				variant_cursor := device.variants.first;
				while variant_cursor /= type_component_variants.no_element loop
					section_mark (section_variant, HEADER);
					write (keyword => keyword_name, space => true, parameters => to_string (key (variant_cursor)));

					query_element (
						position	=> variant_cursor,
						process		=> write_variant'access);

					section_mark (section_variant, FOOTER);					
					next (variant_cursor);
				end loop;

				section_mark (section_variants, FOOTER);
			when others => null;				
		end case;

		-- internal units
		section_mark (section_units_internal, HEADER);
		while unit_internal_cursor /= type_units_internal.no_element loop
			section_mark (section_unit, HEADER);
			query_element (unit_internal_cursor, query_internal_unit'access);
			section_mark (section_unit, FOOTER);
			next (unit_internal_cursor);
		end loop;
		section_mark (section_units_internal, FOOTER);

		-- external units
		section_mark (section_units_external, HEADER);
		while unit_external_cursor /= type_units_external.no_element loop
			section_mark (section_unit, HEADER);
			query_element (unit_external_cursor, query_external_unit'access);
			section_mark (section_unit, FOOTER);
			next (unit_external_cursor);
		end loop;
		section_mark (section_units_external, FOOTER);

		-- write footer
		new_line;		
		put_line (comment_mark & " " & row_separator_double);
		put_line (comment_mark & " device model file end");
		new_line;

		reset_tab_depth;
		
		set_output (standard_output);
		close (file_handle);

		exception when event: others =>
			log (text => ada.exceptions.exception_message (event));
			if is_open (file_handle) then
				close (file_handle);
			end if;
			raise;
		
	end save_device;

	function write_top_level_reached return string is begin return "top level reached"; end;

	function write_enter_section return string is begin return "entering section "; end;

	function write_return_to_section return string is begin return "returning to section "; end;

	function write_missing_begin_end return string is begin 
		return "missing " & section_begin & " or " & section_end & " after section name !"; end;

	function write_section_stack_not_empty return string is begin
		return "section stack not empty !"; end;
	
	procedure invalid_section is 
		use et_string_processing;
	begin
		log (ERROR, "invalid section name !", console => true);
		raise constraint_error;
	end;

	function to_position (
		line : in et_string_processing.type_fields_of_line; -- "keyword x 3 y 4" or "position x 44.5 y 53.5"
		from : in positive)
		return et_coordinates.geometry.type_point is

		use et_string_processing;
		use geometry;

		function f (line : in type_fields_of_line; position : in positive) return string 
			renames et_string_processing.field;
		
		point : et_coordinates.geometry.type_point; -- to be returned

		place : positive := from; -- the field being read from given line

		-- CS: flags to detect missing x or y
	begin
		while place <= positive (field_count (line)) loop

			-- We expect after the x the corresponding value for x
			if f (line, place) = keyword_pos_x then
				--set_x (point, to_distance (f (line, place + 1)));
				set (X, to_distance (f (line, place + 1)), point);

			-- We expect after the y the corresponding value for y
			elsif f (line, place) = keyword_pos_y then
				--set_y (point, to_distance (f (line, place + 1)));
				set (Y, to_distance (f (line, place + 1)),point);

			else
				invalid_keyword (f (line, place));
			end if;
				
			place := place + 2;
		end loop;
		
		return point;
	end to_position;

	function to_alignment (
		line : in et_string_processing.type_fields_of_line; -- "alignment horizontal center vertical center"
		from : in positive)
		return et_libraries.type_text_alignment is
		use et_libraries;
		use et_string_processing;

		function f (line : in type_fields_of_line; position : in positive) return string 
			renames et_string_processing.field;
		
		alignment : type_text_alignment; -- to be returned

		place : positive := from; -- the field being read from given line

		-- CS: flags to detect missing sheet, x or y
	begin
		while place <= positive (field_count (line)) loop

			-- We expect after the "horizontal" the horizontal alignment
			if f (line, place) = keyword_horizontal then
				alignment.horizontal := to_alignment_horizontal (f (line, place + 1));

			-- We expect after the "vertical" the vertical alignment
			elsif f (line, place) = keyword_vertical then
				alignment.vertical := to_alignment_vertical (f (line, place + 1));
				
			else
				invalid_keyword (f (line, place));
			end if;
				
			place := place + 2;
		end loop;
		
		return alignment;
	end to_alignment;

	function to_position ( -- CS combine with next function to_position using the tag test ?
	-- Returns a type_point_2d in the the layout.
		line : in et_string_processing.type_fields_of_line; -- "start x 44.5 y 53.5"
		from : in positive)
		return et_pcb_coordinates.geometry.type_point is
		use et_pcb_coordinates;
		use et_pcb_coordinates.geometry;
		use et_string_processing;

		function f (line : in type_fields_of_line; position : in positive) return string 
			renames et_string_processing.field;
		
		point : et_pcb_coordinates.geometry.type_point; -- to be returned
		place : positive := from; -- the field being read from given line

		-- CS: flags to detect missing sheet, x or y
	begin
		while place <= positive (field_count (line)) loop

			-- We expect after the x the corresponding value for x
			if f (line, place) = keyword_pos_x then
				set (
					point	=> point,
					axis	=> X,
					value 	=> to_distance (f (line, place + 1)));

			-- We expect after the y the corresponding value for y
			elsif f (line, place) = keyword_pos_y then
				set (
					point	=> point,
					axis 	=> Y,
					value 	=> to_distance (f (line, place + 1)));

			else
				invalid_keyword (f (line, place));
			end if;
				
			place := place + 2;
		end loop;
		
		return point;
	end to_position;
		
	function to_position (
	-- Returns a type_position in the layout.
		line : in et_string_processing.type_fields_of_line; -- "x 23 y 0.2 rotation 90.0"
		from : in positive)
		return et_pcb_coordinates.geometry.type_position is

		use et_pcb_coordinates;
		use et_pcb_coordinates.geometry;
		use et_string_processing;
		
		function f (line : in type_fields_of_line; position : in positive) return string 
			renames et_string_processing.field;
		
		point : et_pcb_coordinates.geometry.type_position; -- to be returned
		place : positive := from; -- the field being read from given line

		-- CS: flags to detect missing sheet, x or y
	begin
		while place <= positive (field_count (line)) loop

			-- We expect after the x the corresponding value for x
			if f (line, place) = keyword_pos_x then
				set (point => point, axis => X, value => to_distance (f (line, place + 1)));

			-- We expect after the y the corresponding value for y
			elsif f (line, place) = keyword_pos_y then
				set (point => point, axis => Y, value => to_distance (f (line, place + 1)));

			-- We expect after "rotation" the corresponding value for the rotation
			elsif f (line, place) = keyword_rotation then
				set (point, to_rotation (f (line, place + 1)));
				
			else
				invalid_keyword (f (line, place));
			end if;
				
			place := place + 2;
		end loop;
		
		return point;
	end to_position;

	function to_layers (
	-- Converts a line like "layers 1 4 17" to a set of signal layers.
	-- Issues warning if a layer number occurs more than once.
		line : in et_string_processing.type_fields_of_line) -- layers 1 3 17
		return et_pcb_stack.type_signal_layers.set is

		use et_pcb;
		use et_pcb_stack;
		use et_pcb_stack.type_signal_layers;
		use et_string_processing;

		function f (line : in type_fields_of_line; position : in positive) return string 
			renames et_string_processing.field;
		
		layers 		: type_signal_layers.set; -- to be returned
		cursor 		: type_signal_layers.cursor;
		inserted	: boolean;
		layer 		: type_signal_layer;
		place 		: positive := 2; -- we start reading the layer numbers with field 2
	begin
		while place <= positive (field_count (line)) loop

			-- get the layer number from current place
			layer := to_signal_layer (f (line, place));

			-- insert the layer number in the container "layers"
			insert (
				container	=> layers,
				new_item	=> layer,
				inserted	=> inserted,
				position	=> cursor);

			-- warn if layer already in container
			if not inserted then
				log (WARNING, affected_line (line) & "signal layer" & to_string (layer) 
					& " specified multiple times !");
			end if;
			
			place := place + 1; -- advance to next place
		end loop;
		
		return layers;
	end to_layers;

	function to_dimensions (
		line : in et_string_processing.type_fields_of_line; -- "size width 30 height 40"
		from : in positive)
		return et_packages.type_text_dimensions is
		use et_pcb_coordinates.geometry;
		use et_string_processing;

		function f (line : in type_fields_of_line; position : in positive) return string 
			renames et_string_processing.field;
		
		dim : et_packages.type_text_dimensions; -- to be returned
		place : positive := from; -- the field being read from given line

		-- CS: flags to detect missing x or y
	begin
		while place <= positive (field_count (line)) loop

			-- We expect after the "width" the corresponding value for the text width
			if f (line, place) = keyword_width then
				dim.width := to_distance (f (line, place + 1));

			-- We expect after the "height" the corresponding value for the text height
			elsif f (line, place) = keyword_height then
				dim.height := to_distance (f (line, place + 1));

			else
				invalid_keyword (f (line, place));
			end if;
				
			place := place + 2;
		end loop;
		
		return dim;
	end to_dimensions;

	function to_fillable_circle (
	-- Composes a fillable circle from the given parameters. 
	-- Filled and fill_style are discriminants. Depending on them some parameters
	-- matter or not. See spec for type_fillable_circle.
		circle				: in et_packages.shapes.type_circle;
		filled				: in et_packages.shapes.type_filled;
		fill_style			: in et_packages.type_fill_style;
		circumfence_width	: in et_packages.type_general_line_width;
		hatching			: in et_packages.type_hatching)
		return et_packages.type_fillable_circle is

		use et_packages;
		use et_packages.shapes;

	begin -- to_fillable_circle
		case filled is
			when NO =>
				return (circle with
					filled			=> NO,
					fill_style		=> fill_style,
					border_width	=> circumfence_width);
				
			when YES =>
				case fill_style is
					when SOLID =>
						return (circle with
							filled		=> YES,
							fill_style	=> SOLID);

					when HATCHED =>
						return (circle with
							filled				=> YES,
							fill_style			=> HATCHED,
							hatching			=> hatching);

				end case;
		end case;
	end to_fillable_circle;


	-- This function returns the string at position in given line:
	function f (line : in et_string_processing.type_fields_of_line; position : in positive) return string 
		renames et_string_processing.field;
	

-- BASIC GEOMETRIC OBJECTS USED IN SYMBOLS AND SCHEMATICS
	schematic_object_filled : et_schematic.shapes.type_filled := et_schematic.shapes.filled_default;		
	
	
-- BASIC GEOMETRIC OBJECTS USED IN PACKAGES AND BOARDS
	
	type type_board_line is new et_packages.shapes.type_line with null record;
	board_line : type_board_line;
	procedure board_reset_line is begin board_line := (others => <>); end;

	type type_board_arc is new et_packages.shapes.type_arc with null record;
	board_arc : type_board_arc;
	procedure board_reset_arc is begin board_arc := (others => <>); end;

	type type_board_circle is new et_packages.shapes.type_circle with null record;
	board_circle : type_board_circle;
	procedure board_reset_circle is begin board_circle := (others => <>); end;

	procedure read_board_line (line : et_string_processing.type_fields_of_line) is
	-- Reads start and end point of the board_line. If the statement is invalid then an error issued.
		kw : string := f (line, 1);
		use et_pcb_coordinates.geometry;
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_start then -- start x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the start position starting at field 2 of line
			board_line.start_point := to_position (line, 2);
			
		elsif kw = keyword_end then -- end x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the end position starting at field 2 of line
			board_line.end_point := to_position (line, 2);
			
		else
			invalid_keyword (kw);
		end if;
	end;

	function read_board_line (line : et_string_processing.type_fields_of_line) return boolean is
	-- Reads start and end point of the board_line. If the statement is invalid then it returns a false.
		kw : string := f (line, 1);
		use et_pcb_coordinates.geometry;
	begin
		if kw = keyword_start then -- start x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the start position starting at field 2 of line
			board_line.start_point := to_position (line, 2);
			return true;
			
		elsif kw = keyword_end then -- end x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the end position starting at field 2 of line
			board_line.end_point := to_position (line, 2);
			return true;
		else
			return false;
		end if;
	end;
	
	procedure read_board_arc (line : et_string_processing.type_fields_of_line) is
	-- Reads start and end point of the board_arc. If the statement is invalid then an error issued.
		kw : string := f (line, 1);
		use et_packages.shapes;
		use et_pcb_coordinates.geometry;
	begin
		if kw = keyword_start then -- start x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the start position starting at field 2 of line
			board_arc.start_point := to_position (line, 2);

		elsif kw = keyword_end then -- end x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the end position starting at field 2 of line
			board_arc.end_point := to_position (line, 2);
			
		elsif kw = keyword_center then -- center x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the center position starting at field 2 of line
			board_arc.center := to_position (line, 2);

		else
			invalid_keyword (kw);
		end if;
	end;

	function read_board_arc (line : et_string_processing.type_fields_of_line) return boolean is
	-- Reads start and end point of the board_arc. If the statement is invalid then it returns a false.
		kw : string := f (line, 1);
		use et_packages.shapes;
		use et_pcb_coordinates.geometry;
	begin
		if kw = keyword_start then -- start x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the start position starting at field 2 of line
			board_arc.start_point := to_position (line, 2);

			return true;

		elsif kw = keyword_end then -- end x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the end position starting at field 2 of line
			board_arc.end_point := to_position (line, 2);

			return true;
			
		elsif kw = keyword_center then -- center x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the center position starting at field 2 of line
			board_arc.center := to_position (line, 2);

			return true;
		else
			return false;
		end if;
	end;
	
	procedure read_board_circle (line : et_string_processing.type_fields_of_line) is
	-- Reads start and end point of the board_circle. If the statement is invalid then an error issued.
		kw : string := f (line, 1);
		use et_packages.shapes;
		use et_pcb_coordinates.geometry;
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_center then -- center x 150 y 45
			expect_field_count (line, 5);

			-- extract the center position starting at field 2 of line
			board_circle.center := to_position (line, 2);
			
		elsif kw = keyword_radius then -- radius 22
			expect_field_count (line, 2);
			
			board_circle.radius := et_pcb_coordinates.geometry.to_distance (f (line, 2));
		else
			invalid_keyword (kw);
		end if;
	end;

	function read_board_circle (line : et_string_processing.type_fields_of_line) return boolean is
	-- Reads start and end point of the board_circle. If the statement is invalid then it returns a false.
		kw : string := f (line, 1);
		use et_packages.shapes;
		use et_pcb_coordinates.geometry;
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_center then -- center x 150 y 45
			expect_field_count (line, 5);

			-- extract the center position starting at field 2 of line
			board_circle.center := to_position (line, 2);

			return true;
			
		elsif kw = keyword_radius then -- radius 22
			expect_field_count (line, 2);
			
			board_circle.radius := et_pcb_coordinates.geometry.to_distance (f (line, 2));

			return true;
			
		else
			return false;
		end if;
	end;


	
	board_fill_style : et_packages.type_fill_style := et_packages.fill_style_default;
	board_filled : et_packages.shapes.type_filled := et_packages.shapes.filled_default;

	board_hatching : et_packages.type_hatching;
	board_hatching_copper : et_packages.type_hatching_copper;
	board_easing : et_packages.type_easing;

	
	type type_polygon is new et_packages.shapes.type_polygon_base with null record;
	polygon : type_polygon;
	
	polygon_isolation : et_packages.type_track_clearance := et_packages.type_track_clearance'first;
	polygon_width_min : et_packages.type_track_width := et_packages.type_track_width'first;

	-- board relevant only:
	polygon_pad_connection	: et_pcb.type_polygon_pad_connection := et_pcb.type_polygon_pad_connection'first;
	polygon_priority		: et_pcb.type_polygon_priority := et_pcb.type_polygon_priority'first;
	thermal					: et_pcb.type_thermal;
	signal_layer			: et_pcb_stack.type_signal_layer := et_pcb_stack.type_signal_layer'first;

	procedure board_reset_signal_layer is 
		use et_pcb_stack;
	begin
		signal_layer := type_signal_layer'first;
	end;
	
	board_lock_status		: et_pcb.type_locked := et_pcb.NO;

	procedure board_reset_lock_status is
		use et_pcb;
	begin
		board_lock_status := NO;
	end;
	
	board_line_width : et_packages.type_general_line_width := et_packages.type_general_line_width'first;

	procedure board_reset_line_width is 
		use et_packages;
	begin 
		board_line_width := type_general_line_width'first; 
	end;

	-- package and board relevant:	
	procedure board_reset_circle_fillable is 
		use et_packages;
		use et_packages.shapes;
	begin 
		board_circle		:= (others => <>);
		board_filled		:= type_filled'first;
		board_fill_style	:= fill_style_default;
		board_hatching		:= (others => <>);
		
		board_reset_line_width;
	end;

	function board_make_fillable_circle return et_packages.type_fillable_circle is 
		use et_packages;
	begin
		return to_fillable_circle (
			circle 				=> shapes.type_circle (board_circle),
			filled				=> board_filled,
			fill_style			=> board_fill_style,
			circumfence_width	=> board_line_width,
			hatching			=> board_hatching);
	end;

	function board_make_fillable_circle_solid return et_packages.type_fillable_circle_solid is 
		use et_packages;
	begin
		return (et_packages.shapes.type_circle (board_circle) with board_filled);
	end;

	function board_make_copper_circle return et_packages.type_copper_circle is
		use et_packages;
		use et_packages.shapes;
	begin
		case board_filled is
			when NO =>
				return (shapes.type_circle (board_circle) with 
					filled			=> NO,
					fill_style		=> SOLID, -- don't care here
					border_width	=> board_line_width);

			when YES =>
				case board_fill_style is
					when SOLID =>
						return (shapes.type_circle (board_circle) with 
							filled		=> YES,
							fill_style	=> SOLID);

					when HATCHED =>
						return (shapes.type_circle (board_circle) with
							filled		=> YES,
							fill_style	=> HATCHED,
							hatching 	=> board_hatching_copper);
				end case;
		end case;
	end;
			
	procedure add_polygon_line (line : in out type_board_line) is
		use et_packages.shapes;
		use et_packages.shapes.pac_polygon_lines;

		-- make a polygon line:
		l : type_polygon_line := (et_packages.shapes.type_line (line) with others => <>);
	begin
		-- collect the polygon line 
		append (polygon.segments.lines, l);

		board_reset_line;
	end;

	procedure add_polygon_arc (arc : in out type_board_arc) is
		use et_packages.shapes;
		use et_packages.shapes.pac_polygon_arcs;

		-- make a polygon arc:
		a : type_polygon_arc := (et_packages.shapes.type_arc (arc) with others => <>);
	begin
		-- collect the polygon line 
		append (polygon.segments.arcs, a);

		board_reset_arc;
	end;

	procedure add_polygon_circle (circle : in out type_board_circle) is
		use et_packages.shapes;
		use et_packages.shapes.pac_polygon_circles;

		-- make a polygon circle:
		c : type_polygon_circle := (et_packages.shapes.type_circle (circle) with others => <>);
	begin
		-- collect the polygon line 
		append (polygon.segments.circles, c);

		board_reset_circle;
	end;

	
	procedure board_reset_polygon is
	-- This procdure resets polygon properties to their defaults.
	-- This procdure is used by both package and board parsing procedures read_package and read_module_file.
	-- Some properties have no meaning in packages as remarked below.
		use et_packages;
		use et_packages.shapes;
		use et_pcb_stack;
	begin
		polygon				:= (others => <>);

		board_filled		:= filled_default;
		board_fill_style	:= fill_style_default;
		board_hatching		:= (others => <>);
		board_easing 		:= (others => <>);
		
		polygon_pad_connection	:= et_pcb.type_polygon_pad_connection'first; -- board relevant only
		polygon_priority		:= et_pcb.type_polygon_priority'first;  -- board relevant only
		polygon_isolation		:= et_packages.type_track_clearance'first;
		polygon_width_min		:= type_track_width'first;

		signal_layer			:= type_signal_layer'first;  -- board relevant only

		thermal					:= (others => <>); -- board relevant only
	end;
	
	procedure read_package (
	-- Opens the package file and stores the package in container et_libraries.packages.
		file_name 		: in et_libraries.type_package_model_file.bounded_string; -- libraries/packages/S_SO14.pac
		log_threshold	: in et_string_processing.type_log_level) is
		use et_string_processing;
		use et_packages;
		use et_packages.shapes;
		use et_pcb;
		use et_pcb_coordinates.geometry;
		
		file_handle : ada.text_io.file_type;

		line : et_string_processing.type_fields_of_line;

		-- This is the section stack of the package model. 
		-- Here we track the sections. On entering a section, its name is
		-- pushed onto the stack. When leaving a section the latest section name is popped.
		max_section_depth : constant positive := 7; -- incl. section init
		package stack is new stack_lifo (
			item	=> type_section_name_package,
			max 	=> max_section_depth);

		function to_string (section : in type_section_name_package) return string is
		-- Converts a section like SEC_KEEPOUT to a string "keepout".
			len : positive := type_section_name_package'image (section)'length;
		begin
			return to_lower (type_section_name_package'image (section) (5..len));
		end to_string;

	-- VARIABLES FOR TEMPORARILY STORAGE AND ASSOCIATED HOUSEKEEPING SUBPROGRAMS:

		-- Once the appearance has been read, a new package will be created where this 
		-- pointer is pointing at:
		packge					: access type_package;
		pac_appearance			: type_package_appearance := package_appearance_default;

		-- The description and technology will be assigned once the complete
		-- model has been read. See main of this procedure.
		pac_description			: type_package_description.bounded_string; 
		pac_technology			: type_assembly_technology := assembly_technology_default;
		
		signal_layers : et_pcb_stack.type_signal_layers.set;
		
-- 		pac_circle_copper		: et_packages.type_copper_circle;
-- 		procedure reset_circle_copper is begin pac_circle_copper := (others => <>); end;		

		pac_text				: et_packages.type_text_with_content;
		pac_text_placeholder	: et_packages.type_text_placeholder;


		terminal_position		: et_pcb_coordinates.geometry.type_position := origin_zero_rotation;

		tht_width_inner_layers	: et_pcb_coordinates.type_distance := zero; -- CS rework
		tht_hole				: et_packages.type_terminal_tht_hole := et_packages.terminal_tht_hole_default;
		tht_drill_size			: et_packages.type_drill_size := et_packages.type_drill_size'first;
		tht_millings			: et_packages.type_plated_millings;

		terminal_name			: et_libraries.type_terminal_name.bounded_string;
		terminal_technology		: et_packages.type_assembly_technology := et_packages.assembly_technology_default;
		tht_pad_shape			: et_packages.type_pad_outline_tht;		
		smt_pad_shape			: et_packages.type_pad_outline;

		smt_pad_face			: et_pcb_coordinates.type_face := et_pcb_coordinates.face_default;
		smt_stop_mask			: et_packages.type_stop_mask_status := et_packages.stop_mask_status_default;
		smt_solder_paste		: et_packages.type_solder_paste_status := et_packages.solder_paste_status_default;

		procedure build_terminal is 
		-- Assembles the elements of a terminal and appends the final terminal to the
		-- list of terminals of the package.
			cursor : type_terminals.cursor;
			inserted : boolean;
		begin
			case terminal_technology is
				when THT => 
					case tht_hole is
						when DRILLED =>

							type_terminals.insert (
								container	=> packge.terminals,
								key			=> terminal_name, -- 1,4,16
								position	=> cursor,
								inserted	=> inserted,
								new_item	=> (
									technology			=> THT,
									tht_hole			=> DRILLED,
									drill_size			=> tht_drill_size,
									position			=> terminal_position,
									pad_shape_tht		=> tht_pad_shape,
									width_inner_layers	=> tht_width_inner_layers));

						when MILLED =>
							type_terminals.insert (
								container	=> packge.terminals,
								key			=> terminal_name, -- 1,4,16
								position	=> cursor,
								inserted	=> inserted,
								new_item	=> (
									technology			=> THT,
									tht_hole			=> MILLED,
									millings			=> tht_millings,
									position			=> terminal_position,
									pad_shape_tht		=> tht_pad_shape,
									width_inner_layers	=> tht_width_inner_layers));
					end case;

				when SMT =>
					type_terminals.insert (
						container	=> packge.terminals,
						key			=> terminal_name, -- 1,4,16,H9
						position	=> cursor,
						inserted	=> inserted,
						new_item	=> (
							technology		=> SMT,
							tht_hole		=> terminal_tht_hole_default, -- not relevant here, see spec
							face			=> smt_pad_face,
							position		=> terminal_position,
							pad_shape		=> smt_pad_shape,
							stop_mask		=> smt_stop_mask,
							solder_paste	=> smt_solder_paste));

			end case;

			if not inserted then
				log (ERROR, "terminal" & et_libraries.to_string (terminal_name) 
					 & " already used !", console => true);
				raise constraint_error;
			end if;

			-- clean up for next terminal
			terminal_position := origin_zero_rotation;
 			smt_pad_shape := (others => <>);
			smt_stop_mask := et_packages.stop_mask_status_default;
			smt_solder_paste := solder_paste_status_default;
			tht_pad_shape := (others => <>);
			tht_hole := terminal_tht_hole_default;
			tht_width_inner_layers := zero;
			tht_drill_size := type_drill_size'first;
			
		end build_terminal;
		
		procedure process_line is 

			procedure execute_section is
			-- Once a section concludes, the temporarily variables are read, evaluated
				-- and finally assembled to actual objects:

				-- fill zones
				procedure append_silk_polygon_top is begin
					case board_fill_style is
						when SOLID =>
							pac_silk_polygons.append (
								container	=> packge.silk_screen.top.polygons, 
								new_item	=> (shapes.type_polygon_base (polygon) with
												fill_style 	=> SOLID,
												easing 		=> board_easing
											   ));

						when HATCHED =>
							pac_silk_polygons.append (
								container	=> packge.silk_screen.top.polygons, 
								new_item	=> (shapes.type_polygon_base (polygon) with 
												fill_style 	=> HATCHED,
												easing 		=> board_easing,
												hatching	=> board_hatching));
					end case;
					
					board_reset_polygon;
				end;

				procedure append_silk_polygon_bottom is begin
					case board_fill_style is
						when SOLID =>
							pac_silk_polygons.append (
								container	=> packge.silk_screen.bottom.polygons, 
								new_item	=> (shapes.type_polygon_base (polygon) with 
												fill_style	=> SOLID,
												easing		=> board_easing
											   ));

						when HATCHED =>
							pac_silk_polygons.append (
								container	=> packge.silk_screen.bottom.polygons, 
								new_item	=> (shapes.type_polygon_base (polygon) with 
												fill_style	=> HATCHED,
												easing		=> board_easing,
												hatching	=> board_hatching));
					end case;
					
					-- clean up for next polygon
					board_reset_polygon;
				end;

				procedure append_assy_doc_polygon_top is begin
					case board_fill_style is
						when SOLID =>
							pac_doc_polygons.append (
								container	=> packge.assembly_documentation.top.polygons, 
								new_item	=> (shapes.type_polygon_base (polygon) with 
												easing		=> board_easing,
												fill_style	=> SOLID));

						when HATCHED =>
							pac_doc_polygons.append (
								container	=> packge.assembly_documentation.top.polygons, 
								new_item	=> (shapes.type_polygon_base (polygon) with 
												fill_style	=> HATCHED,
												easing		=> board_easing,
												hatching 	=> board_hatching));
					end case;
					
					-- clean up for next polygon
					board_reset_polygon;
				end;

				procedure append_assy_doc_polygon_bottom is begin
					case board_fill_style is
						when SOLID =>
							pac_doc_polygons.append (
								container	=> packge.assembly_documentation.bottom.polygons, 
								new_item	=> (shapes.type_polygon_base (polygon) with 
												easing 		=> board_easing,
												fill_style	=> SOLID));

						when HATCHED =>
							pac_doc_polygons.append (
								container	=> packge.assembly_documentation.bottom.polygons, 
								new_item	=> (shapes.type_polygon_base (polygon) with 
												fill_style	=> HATCHED,
												easing		=> board_easing,
												hatching	=> board_hatching));

					end case;
					
					-- clean up for next polygon
					board_reset_polygon;
				end;

				procedure append_keepout_polygon_top is begin
					type_keepout_polygons.append (
						container	=> packge.keepout.top.polygons, 
						new_item	=> (shapes.type_polygon_base (polygon) with 
										filled	=> board_filled));

					-- clean up for next polygon
					board_reset_polygon;
				end;

				procedure append_keepout_polygon_bottom is begin
					type_keepout_polygons.append (
						container	=> packge.keepout.bottom.polygons, 
						new_item	=> (shapes.type_polygon_base (polygon) with
										filled	=> board_filled));

					-- clean up for next polygon
					board_reset_polygon;
				end;

				procedure append_stencil_polygon_top is begin
					case board_fill_style is
						when SOLID =>
							type_stencil_polygons.append (
								container	=> packge.stencil.top.polygons, 
								new_item	=> (shapes.type_polygon_base (polygon) with
										fill_style	=> SOLID,
										easing		=> board_easing));

						when HATCHED =>
							type_stencil_polygons.append (
								container	=> packge.stencil.top.polygons, 
								new_item	=> (shapes.type_polygon_base (polygon) with
										fill_style	=> HATCHED,
										easing		=> board_easing,
										hatching	=> board_hatching));
					end case;

					-- clean up for next polygon
					board_reset_polygon;
				end;

				procedure append_stencil_polygon_bottom is begin
					case board_fill_style is
						when SOLID =>
							type_stencil_polygons.append (
								container	=> packge.stencil.bottom.polygons, 
								new_item	=> (shapes.type_polygon_base (polygon) with
										fill_style	=> SOLID,
										easing		=> board_easing));

						when HATCHED =>
							type_stencil_polygons.append (
								container	=> packge.stencil.bottom.polygons, 
								new_item	=> (shapes.type_polygon_base (polygon) with
										fill_style	=> HATCHED,
										easing		=> board_easing,
										hatching	=> board_hatching));
					end case;

					-- clean up for next polygon
					board_reset_polygon;
				end;

				procedure append_stop_polygon_top is begin
					case board_fill_style is
						when SOLID =>
							type_stop_polygons.append (
								container	=> packge.stop_mask.top.polygons, 
								new_item	=> (shapes.type_polygon_base (polygon) with
										fill_style	=> SOLID,
										easing		=> board_easing));

						when HATCHED =>
							type_stop_polygons.append (
								container	=> packge.stop_mask.top.polygons, 
								new_item	=> (shapes.type_polygon_base (polygon) with
										fill_style	=> HATCHED,
										easing		=> board_easing,
										hatching	=> board_hatching));
					end case;

					-- clean up for next polygon
					board_reset_polygon;
				end;
				
				procedure append_stop_polygon_bottom is begin
					case board_fill_style is
						when SOLID =>
							type_stop_polygons.append (
								container	=> packge.stop_mask.bottom.polygons, 
								new_item	=> (shapes.type_polygon_base (polygon) with
										fill_style	=> SOLID,
										easing		=> board_easing));

						when HATCHED =>
							type_stop_polygons.append (
								container	=> packge.stop_mask.bottom.polygons, 
								new_item	=> (shapes.type_polygon_base (polygon) with
										fill_style	=> HATCHED,
										easing		=> board_easing,
										hatching	=> board_hatching));
					end case;

					-- clean up for next polygon
					board_reset_polygon;
				end;

				procedure append_copper_polygon_top is begin
					case board_fill_style is
						when SOLID =>
							pac_copper_polygons_solid.append (
								container	=> packge.copper.top.polygons.solid, 
								new_item	=> (shapes.type_polygon_base (polygon) with
										fill_style	=> SOLID,
										easing		=> board_easing,
										width_min 	=> polygon_width_min,
										isolation	=> polygon_isolation));

						when HATCHED =>
							pac_copper_polygons_hatched.append (
								container	=> packge.copper.top.polygons.hatched, 
								new_item	=> (shapes.type_polygon_base (polygon) with
										fill_style	=> HATCHED,
										easing		=> board_easing,
										hatching	=> board_hatching,
										width_min 	=> polygon_width_min,
										isolation	=> polygon_isolation));
					end case;
										
					-- clean up for next polygon
					board_reset_polygon;
				end;

				procedure append_copper_polygon_bottom is begin
					case board_fill_style is
						when SOLID =>
							pac_copper_polygons_solid.append (
								container	=> packge.copper.bottom.polygons.solid, 
								new_item	=> (shapes.type_polygon_base (polygon) with
										fill_style	=> SOLID,
										easing		=> board_easing,
										width_min 	=> polygon_width_min,
										isolation	=> polygon_isolation));

						when HATCHED =>
							pac_copper_polygons_hatched.append (
								container	=> packge.copper.bottom.polygons.hatched, 
								new_item	=> (shapes.type_polygon_base (polygon) with
										fill_style	=> HATCHED,
										easing		=> board_easing,
										hatching	=> board_hatching,
										width_min 	=> polygon_width_min,
										isolation	=> polygon_isolation));
					end case;
										
					-- clean up for next polygon
					board_reset_polygon;
				end;

				procedure append_route_restrict_polygon is begin
					type_route_restrict_polygons.append (
						container	=> packge.route_restrict.polygons, 
						new_item	=> (shapes.type_polygon_base (polygon) with 
										filled	=> board_filled,
										layers	=> signal_layers));

					-- clean up for next polygon
					board_reset_polygon;

					et_pcb_stack.type_signal_layers.clear (signal_layers);
				end;

				procedure append_via_restrict_polygon is begin
					type_via_restrict_polygons.append (
						container	=> packge.via_restrict.polygons, 
						new_item	=> (shapes.type_polygon_base (polygon) with 
										filled	=> board_filled,
										layers	=> signal_layers));

					-- clean up for next polygon
					board_reset_polygon;

					et_pcb_stack.type_signal_layers.clear (signal_layers);
				end;

				-- cutout zones
				procedure append_silk_cutout_top is begin
					pac_silk_cutouts.append (
						container	=> packge.silk_screen.top.cutouts, 
						new_item	=> (shapes.type_polygon_base (polygon) with
										easing => board_easing));
					
					-- clean up for next polygon
					board_reset_polygon;
				end;

				procedure append_silk_cutout_bottom is begin
					pac_silk_cutouts.append (
						container	=> packge.silk_screen.bottom.cutouts, 
						new_item	=> (shapes.type_polygon_base (polygon) with
										easing => board_easing));
					
					-- clean up for next polygon
					board_reset_polygon;
				end;

				procedure append_assy_doc_cutout_top is begin
					pac_doc_cutouts.append (
						container	=> packge.assembly_documentation.top.cutouts, 
						new_item	=> (shapes.type_polygon_base (polygon) with 
										easing => board_easing));
					
					-- clean up for next polygon
					board_reset_polygon;
				end;

				procedure append_assy_doc_cutout_bottom is begin
					pac_doc_cutouts.append (
						container	=> packge.assembly_documentation.bottom.cutouts, 
						new_item	=> (shapes.type_polygon_base (polygon) with 
										easing => board_easing));
					
					-- clean up for next polygon
					board_reset_polygon;
				end;

				procedure append_keepout_cutout_top is begin
					pac_keepout_cutouts.append (
						container	=> packge.keepout.top.cutouts, 
						new_item	=> (shapes.type_polygon_base (polygon) with 
										easing => board_easing));

					-- clean up for next polygon
					board_reset_polygon;
				end;

				procedure append_keepout_cutout_bottom is begin
					pac_keepout_cutouts.append (
						container	=> packge.keepout.bottom.cutouts, 
						new_item	=> (shapes.type_polygon_base (polygon) with 
										easing => board_easing));

					-- clean up for next polygon
					board_reset_polygon;
				end;

				procedure append_stencil_cutout_top is begin
					pac_stencil_cutouts.append (
						container	=> packge.stencil.top.cutouts, 
						new_item	=> (shapes.type_polygon_base (polygon) with
										easing => board_easing));

					-- clean up for next polygon
					board_reset_polygon;
				end;

				procedure append_stencil_cutout_bottom is begin
					pac_stencil_cutouts.append (
						container	=> packge.stencil.top.cutouts, 
						new_item	=> (shapes.type_polygon_base (polygon) with
										easing => board_easing));

					-- clean up for next polygon
					board_reset_polygon;
				end;

				procedure append_stop_cutout_top is begin
					pac_stop_cutouts.append (
						container	=> packge.stop_mask.top.cutouts, 
						new_item	=> (shapes.type_polygon_base (polygon) with
										easing => board_easing));

					-- clean up for next polygon
					board_reset_polygon;
				end;
				
				procedure append_stop_cutout_bottom is begin
					pac_stop_cutouts.append (
						container	=> packge.stop_mask.bottom.cutouts, 
						new_item	=> (shapes.type_polygon_base (polygon) with
										easing => board_easing));

					-- clean up for next polygon
					board_reset_polygon;
				end;

				procedure append_copper_cutout_top is begin
					et_packages.pac_copper_cutouts.append (
						container	=> packge.copper.top.cutouts, 
						new_item	=> (shapes.type_polygon_base (polygon) with
										easing => board_easing));
										
					-- clean up for next polygon
					board_reset_polygon;
				end;

				procedure append_copper_cutout_bottom is begin
					et_packages.pac_copper_cutouts.append (
						container	=> packge.copper.bottom.cutouts, 
						new_item	=> (shapes.type_polygon_base (polygon) with
										easing => board_easing));
										
					-- clean up for next polygon
					board_reset_polygon;
				end;

				procedure append_route_restrict_cutout is begin
					pac_route_restrict_cutouts.append (
						container	=> packge.route_restrict.cutouts, 
						new_item	=> (shapes.type_polygon_base (polygon) with 
										easing => board_easing,
										layers => signal_layers));

					-- clean up for next polygon
					board_reset_polygon;

					et_pcb_stack.type_signal_layers.clear (signal_layers);
				end;

				procedure append_via_restrict_cutout is begin
					pac_via_restrict_cutouts.append (
						container	=> packge.via_restrict.cutouts, 
						new_item	=> (shapes.type_polygon_base (polygon) with 
										easing => board_easing,
										layers => signal_layers));

					-- clean up for next polygon
					board_reset_polygon;

					et_pcb_stack.type_signal_layers.clear (signal_layers);
				end;

			begin -- execute_section
				case stack.current is

					when SEC_COPPER | SEC_KEEPOUT | SEC_STOP_MASK | SEC_STENCIL | 
						SEC_SILK_SCREEN | SEC_ASSEMBLY_DOCUMENTATION |
						SEC_ROUTE_RESTRICT | SEC_VIA_RESTRICT | SEC_PCB_CONTOURS_NON_PLATED | 
						SEC_TERMINALS | SEC_PACKAGE_3D_CONTOURS =>

						case stack.parent is
							when SEC_INIT => null;
							when others => invalid_section;
						end case;

					when SEC_TOP =>
						case stack.parent is
							when SEC_COPPER | SEC_KEEPOUT | SEC_STOP_MASK | SEC_STENCIL | 
								SEC_SILK_SCREEN | SEC_ASSEMBLY_DOCUMENTATION => null;

							when SEC_PAD_CONTOURS_THT => 
								tht_pad_shape.top := (shapes.type_polygon_base (polygon) with null record);
								board_reset_polygon;

							when others => invalid_section;
						end case;
						
					when SEC_BOTTOM =>
						case stack.parent is
							when SEC_COPPER | SEC_KEEPOUT | SEC_STOP_MASK | SEC_STENCIL | 
								SEC_SILK_SCREEN | SEC_ASSEMBLY_DOCUMENTATION => null;

							when SEC_PAD_CONTOURS_THT =>
								tht_pad_shape.bottom := (shapes.type_polygon_base (polygon) with null record);
								board_reset_polygon;
								
							when others => invalid_section;
						end case;
						
					when SEC_LINE =>
						case stack.parent is
							when SEC_TOP => 
								case stack.parent (degree => 2) is
									when SEC_COPPER => -- NON-ELECTRIC !!

										type_copper_lines.append (
											container	=> packge.copper.top.lines, 
											new_item	=> (shapes.type_line (board_line) with board_line_width));

										-- clean up for next line
										board_reset_line;
										board_reset_line_width;

									when SEC_SILK_SCREEN => 
										type_silk_lines.append (
											container	=> packge.silk_screen.top.lines, 
											new_item	=> (shapes.type_line (board_line) with board_line_width));

										-- clean up for next line
										board_reset_line;
										board_reset_line_width;

									when SEC_ASSEMBLY_DOCUMENTATION =>
										type_doc_lines.append (
											container	=> packge.assembly_documentation.top.lines, 
											new_item	=> (shapes.type_line (board_line) with board_line_width));

										-- clean up for next line
										board_reset_line;
										board_reset_line_width;

									when SEC_STENCIL =>
										type_stencil_lines.append (
											container	=> packge.stencil.top.lines, 
											new_item	=> (shapes.type_line (board_line) with board_line_width));

										-- clean up for next line
										board_reset_line;
										board_reset_line_width;

									when SEC_STOP_MASK =>
										type_stop_lines.append (
											container	=> packge.stop_mask.top.lines, 
											new_item	=> (shapes.type_line (board_line) with board_line_width));

										-- clean up for next line
										board_reset_line;
										board_reset_line_width;

									when SEC_KEEPOUT =>
										type_keepout_lines.append (
											container	=> packge.keepout.top.lines, 
											new_item	=> (shapes.type_line (board_line) with null record));

										-- clean up for next line
										board_reset_line;
										
									when SEC_PAD_CONTOURS_THT => add_polygon_line (board_line);
										
									when others => invalid_section;
								end case;

							when SEC_BOTTOM => 
								case stack.parent (degree => 2) is
									when SEC_COPPER => -- NON-ELECTRIC !!

										type_copper_lines.append (
											container	=> packge.copper.bottom.lines, 
											new_item	=> (shapes.type_line (board_line) with board_line_width));

										-- clean up for next line
										board_reset_line;
										board_reset_line_width;

									when SEC_SILK_SCREEN => 
										type_silk_lines.append (
											container	=> packge.silk_screen.bottom.lines, 
											new_item	=> (shapes.type_line (board_line) with board_line_width));

										-- clean up for next line
										board_reset_line;
										board_reset_line_width;
										
									when SEC_ASSEMBLY_DOCUMENTATION =>
										type_doc_lines.append (
											container	=> packge.assembly_documentation.bottom.lines, 
											new_item	=> (shapes.type_line (board_line) with board_line_width));

										-- clean up for next line
										board_reset_line;
										board_reset_line_width;

									when SEC_STENCIL =>
										type_stencil_lines.append (
											container	=> packge.stencil.bottom.lines, 
											new_item	=> (shapes.type_line (board_line) with board_line_width));

										-- clean up for next line
										board_reset_line;
										board_reset_line_width;
										
									when SEC_STOP_MASK =>
										type_stop_lines.append (
											container	=> packge.stop_mask.bottom.lines, 
											new_item	=> (shapes.type_line (board_line) with board_line_width));

										-- clean up for next line
										board_reset_line;
										board_reset_line_width;

									when SEC_KEEPOUT =>
										type_keepout_lines.append (
											container	=> packge.keepout.bottom.lines, 
											new_item	=> (shapes.type_line (board_line) with null record));

										-- clean up for next line
										board_reset_line;

									when SEC_PAD_CONTOURS_THT => add_polygon_line (board_line);

									when others => invalid_section;
								end case;
								
							when SEC_PCB_CONTOURS_NON_PLATED =>
								
								et_packages.type_pcb_contour_lines.append (
									container	=> packge.pcb_contour.lines,
									new_item	=> (shapes.type_line (board_line) with null record));

								-- clean up for next line
								board_reset_line;
								
							when SEC_ROUTE_RESTRICT =>
								
								type_route_restrict_lines.append (
									container	=> packge.route_restrict.lines,
									new_item	=> (shapes.type_line (board_line) with
													layers	=> signal_layers));

								-- clean up for next line
								board_reset_line;
								et_pcb_stack.type_signal_layers.clear (signal_layers);

							when SEC_VIA_RESTRICT =>
								
								type_via_restrict_lines.append (
									container	=> packge.via_restrict.lines,
									new_item	=> (shapes.type_line (board_line) with
													layers	=> signal_layers));

								-- clean up for next line
								board_reset_line;
								et_pcb_stack.type_signal_layers.clear (signal_layers);
								
							when SEC_PAD_CONTOURS_SMT => add_polygon_line (board_line);

							when SEC_MILLINGS => add_polygon_line (board_line);

							when SEC_CONTOURS => add_polygon_line (board_line);
								
							when others => invalid_section;
						end case;
						
					when SEC_ARC =>
						case stack.parent is
							when SEC_TOP => 
								case stack.parent (degree => 2) is
									when SEC_COPPER => -- NON-ELECTRIC !!

										type_copper_arcs.append (
											container	=> packge.copper.top.arcs, 
											new_item	=> (shapes.type_arc (board_arc) with board_line_width));

										-- clean up for next arc
										board_reset_arc;
										board_reset_line_width;

									when SEC_SILK_SCREEN => 
										type_silk_arcs.append (
											container	=> packge.silk_screen.top.arcs, 
											new_item	=> (shapes.type_arc (board_arc) with board_line_width));

										-- clean up for next arc
										board_reset_arc;
										board_reset_line_width;

									when SEC_ASSEMBLY_DOCUMENTATION =>
										type_doc_arcs.append (
											container	=> packge.assembly_documentation.top.arcs, 
											new_item	=> (shapes.type_arc (board_arc) with board_line_width));

										-- clean up for next arc
										board_reset_arc;
										board_reset_line_width;

									when SEC_STENCIL =>
										type_stencil_arcs.append (
											container	=> packge.stencil.top.arcs, 
											new_item	=> (shapes.type_arc (board_arc) with board_line_width));

										-- clean up for next arc
										board_reset_arc;
										board_reset_line_width;

									when SEC_STOP_MASK =>
										type_stop_arcs.append (
											container	=> packge.stop_mask.top.arcs, 
											new_item	=> (shapes.type_arc (board_arc) with board_line_width));

										-- clean up for next arc
										board_reset_arc;
										board_reset_line_width;

									when SEC_KEEPOUT =>
										type_keepout_arcs.append (
											container	=> packge.keepout.top.arcs,
											new_item	=> (shapes.type_arc (board_arc) with null record));

										-- clean up for next arc
										board_reset_arc;

									when SEC_PAD_CONTOURS_THT => add_polygon_arc (board_arc);
										
									when others => invalid_section;
								end case;

							when SEC_BOTTOM => 
								case stack.parent (degree => 2) is
									when SEC_COPPER => -- NON-ELECTRIC !!

										type_copper_arcs.append (
											container	=> packge.copper.bottom.arcs, 
											new_item	=> (shapes.type_arc (board_arc) with board_line_width));

										-- clean up for next arc
										board_reset_arc;
										board_reset_line_width;

									when SEC_SILK_SCREEN => 
										type_silk_arcs.append (
											container	=> packge.silk_screen.bottom.arcs, 
											new_item	=> (shapes.type_arc (board_arc) with board_line_width));

										-- clean up for next arc
										board_reset_arc;
										board_reset_line_width;
										
									when SEC_ASSEMBLY_DOCUMENTATION =>
										type_doc_arcs.append (
											container	=> packge.assembly_documentation.bottom.arcs, 
											new_item	=> (shapes.type_arc (board_arc) with board_line_width));

										-- clean up for next arc
										board_reset_arc;
										board_reset_line_width;

									when SEC_STENCIL =>
										type_stencil_arcs.append (
											container	=> packge.stencil.bottom.arcs, 
											new_item	=> (shapes.type_arc (board_arc) with board_line_width));

										-- clean up for next arc
										board_reset_arc;
										board_reset_line_width;
										
									when SEC_STOP_MASK =>
										type_stop_arcs.append (
											container	=> packge.stop_mask.bottom.arcs, 
											new_item	=> (shapes.type_arc (board_arc) with board_line_width));

										-- clean up for next arc
										board_reset_arc;
										board_reset_line_width;

									when SEC_KEEPOUT =>
										type_keepout_arcs.append (
											container	=> packge.keepout.bottom.arcs, 
											new_item	=> (shapes.type_arc (board_arc) with null record));

										-- clean up for next arc
										board_reset_arc;

									when SEC_PAD_CONTOURS_THT => add_polygon_arc (board_arc);
										
									when others => invalid_section;
								end case;

							when SEC_PCB_CONTOURS_NON_PLATED =>
								
								et_packages.type_pcb_contour_arcs.append (
									container	=> packge.pcb_contour.arcs,
									new_item	=> (shapes.type_arc (board_arc) with null record));

								-- clean up for next arc
								board_reset_arc;
								
							when SEC_ROUTE_RESTRICT =>
								
								type_route_restrict_arcs.append (
									container	=> packge.route_restrict.arcs,
									new_item	=> (shapes.type_arc (board_arc) with layers => signal_layers));

								-- clean up for next arc
								board_reset_arc;
								et_pcb_stack.type_signal_layers.clear (signal_layers);

							when SEC_VIA_RESTRICT =>
								
								type_via_restrict_arcs.append (
									container	=> packge.via_restrict.arcs,
									new_item	=> (shapes.type_arc (board_arc) with layers => signal_layers));

								-- clean up for next arc
								board_reset_arc;
								et_pcb_stack.type_signal_layers.clear (signal_layers);

							when SEC_PAD_CONTOURS_SMT => add_polygon_arc (board_arc);

							when SEC_MILLINGS => add_polygon_arc (board_arc);

							when SEC_CONTOURS => add_polygon_arc (board_arc);
								
							when others => invalid_section;
						end case;

					when SEC_CIRCLE =>
						case stack.parent is
							when SEC_TOP => 
								case stack.parent (degree => 2) is
									when SEC_COPPER => -- NON-ELECTRIC !!

										et_packages.pac_copper_circles.append (
											container	=> packge.copper.top.circles, 
											new_item	=> board_make_copper_circle);

									when SEC_SILK_SCREEN => 
										type_silk_circles.append (
											container	=> packge.silk_screen.top.circles, 
											new_item	=> board_make_fillable_circle);
															
										board_reset_circle_fillable; -- clean up for next circle

									when SEC_ASSEMBLY_DOCUMENTATION =>
										type_doc_circles.append (
											container	=> packge.assembly_documentation.top.circles, 
											new_item	=> board_make_fillable_circle);

										board_reset_circle_fillable; -- clean up for next circle
										
									when SEC_STENCIL =>
										type_stencil_circles.append (
											container	=> packge.stencil.top.circles, 
											new_item	=> board_make_fillable_circle);

										board_reset_circle_fillable; -- clean up for next circle
										
									when SEC_STOP_MASK =>
										type_stop_circles.append (
											container	=> packge.stop_mask.top.circles, 
											new_item	=> board_make_fillable_circle);

										board_reset_circle_fillable; -- clean up for next circle

									when SEC_KEEPOUT =>
										type_keepout_circles.append (
											container	=> packge.keepout.top.circles,
											new_item	=> board_make_fillable_circle_solid);

										board_reset_circle_fillable; -- clean up for next circle

									when SEC_PAD_CONTOURS_THT => add_polygon_circle (board_circle);
										
									when others => invalid_section;
								end case;

							when SEC_BOTTOM =>
								case stack.parent (degree => 2) is
									when SEC_COPPER => -- NON-ELECTRIC !!

										et_packages.pac_copper_circles.append (
											container	=> packge.copper.bottom.circles, 
											new_item	=> board_make_copper_circle);

									when SEC_SILK_SCREEN => 
										type_silk_circles.append (
											container	=> packge.silk_screen.bottom.circles, 
											new_item	=> board_make_fillable_circle);

										board_reset_circle_fillable; -- clean up for next circle
										
									when SEC_ASSEMBLY_DOCUMENTATION =>
										type_doc_circles.append (
											container	=> packge.assembly_documentation.bottom.circles, 
											new_item	=> board_make_fillable_circle);

										board_reset_circle_fillable; -- clean up for next circle

									when SEC_STENCIL =>
										type_stencil_circles.append (
											container	=> packge.stencil.bottom.circles, 
											new_item	=> board_make_fillable_circle);

										board_reset_circle_fillable; -- clean up for next circle

									when SEC_STOP_MASK =>
										type_stop_circles.append (
											container	=> packge.stop_mask.bottom.circles, 
											new_item	=> board_make_fillable_circle);

										board_reset_circle_fillable; -- clean up for next circle

									when SEC_KEEPOUT =>
										type_keepout_circles.append (
											container	=> packge.keepout.bottom.circles,
											new_item	=> board_make_fillable_circle_solid);

										board_reset_circle_fillable; -- clean up for next circle

									when SEC_PAD_CONTOURS_THT => add_polygon_circle (board_circle);
										
									when others => invalid_section;
								end case;

							when SEC_PCB_CONTOURS_NON_PLATED =>
								
								et_packages.type_pcb_contour_circles.append (
									container	=> packge.pcb_contour.circles,
									new_item	=> (shapes.type_circle (board_circle) with null record));

								-- clean up for next circle
								board_reset_circle;
								
							when SEC_ROUTE_RESTRICT =>
								
								type_route_restrict_circles.append (
									container	=> packge.route_restrict.circles,
									new_item	=> (board_make_fillable_circle_solid with signal_layers));

								board_reset_circle_fillable; -- clean up for next circle
								et_pcb_stack.type_signal_layers.clear (signal_layers);

							when SEC_VIA_RESTRICT =>
								
								type_via_restrict_circles.append (
									container	=> packge.via_restrict.circles,
									new_item	=> (board_make_fillable_circle_solid with signal_layers));

								board_reset_circle_fillable; -- clean up for next circle
								et_pcb_stack.type_signal_layers.clear (signal_layers);

							when SEC_PAD_CONTOURS_SMT => add_polygon_circle (board_circle);

							when SEC_MILLINGS => add_polygon_circle (board_circle);

							when SEC_CONTOURS => add_polygon_circle (board_circle);
								
							when others => invalid_section;
						end case;

					when SEC_FILL_ZONE =>
						case stack.parent is
							when SEC_TOP => 
								case stack.parent (degree => 2) is
									when SEC_SILK_SCREEN =>
										append_silk_polygon_top;
										
									when SEC_ASSEMBLY_DOCUMENTATION =>
										append_assy_doc_polygon_top;
										
									when SEC_STENCIL =>
										append_stencil_polygon_top;
										
									when SEC_STOP_MASK =>
										append_stop_polygon_top;
										
									when SEC_KEEPOUT =>
										append_keepout_polygon_top;

									when SEC_COPPER =>
										append_copper_polygon_top;
										
									when others => invalid_section;
								end case;

							when SEC_BOTTOM => 
								case stack.parent (degree => 2) is
									when SEC_SILK_SCREEN =>
										append_silk_polygon_bottom;
										
									when SEC_ASSEMBLY_DOCUMENTATION =>
										append_assy_doc_polygon_bottom;
										
									when SEC_STENCIL =>
										append_stencil_polygon_bottom;
										
									when SEC_STOP_MASK =>
										append_stop_polygon_bottom;
										
									when SEC_KEEPOUT =>
										append_keepout_polygon_bottom;

									when SEC_COPPER =>
										append_copper_polygon_bottom;
										
									when others => invalid_section;
								end case;
								
							when SEC_ROUTE_RESTRICT =>
								append_route_restrict_polygon;

							when SEC_VIA_RESTRICT =>
								append_via_restrict_polygon;

							when others => invalid_section;
						end case;

					when SEC_CUTOUT_ZONE =>
						case stack.parent is
							when SEC_TOP => 
								case stack.parent (degree => 2) is
									when SEC_SILK_SCREEN =>
										append_silk_cutout_top;
										
									when SEC_ASSEMBLY_DOCUMENTATION =>
										append_assy_doc_cutout_top;
										
									when SEC_STENCIL =>
										append_stencil_cutout_top;
										
									when SEC_STOP_MASK =>
										append_stop_cutout_top;
										
									when SEC_KEEPOUT =>
										append_keepout_cutout_top;

									when SEC_COPPER =>
										append_copper_cutout_top;

									when others => invalid_section;
								end case;

							when SEC_BOTTOM => 
								case stack.parent (degree => 2) is
									when SEC_SILK_SCREEN =>
										append_silk_cutout_bottom;
										
									when SEC_ASSEMBLY_DOCUMENTATION =>
										append_assy_doc_cutout_bottom;
										
									when SEC_STENCIL =>
										append_stencil_cutout_bottom;
										
									when SEC_STOP_MASK =>
										append_stop_cutout_bottom;
										
									when SEC_KEEPOUT =>
										append_keepout_cutout_bottom;

									when SEC_COPPER =>
										append_copper_cutout_bottom;

									when others => invalid_section;
								end case;
								
							when SEC_ROUTE_RESTRICT =>
								append_route_restrict_cutout;

							when SEC_VIA_RESTRICT =>
								append_via_restrict_cutout;
								
							when others => invalid_section;
						end case;
						
					when SEC_CONTOURS =>
						case stack.parent is
							when SEC_FILL_ZONE => null;
							when SEC_CUTOUT_ZONE => null;
							when others => invalid_section;
						end case;

					when SEC_TEXT =>
						case stack.parent is
							when SEC_TOP =>
								case stack.parent (degree => 2) is
									when SEC_COPPER =>
										
										type_texts_with_content.append (
											container	=> packge.copper.top.texts,
											new_item	=> pac_text);

									when SEC_SILK_SCREEN =>

										type_texts_with_content.append (
											container	=> packge.silk_screen.top.texts,
											new_item	=> pac_text);


									when SEC_ASSEMBLY_DOCUMENTATION =>

										type_texts_with_content.append (
											container	=> packge.assembly_documentation.top.texts,
											new_item	=> pac_text);
										
									when SEC_STOP_MASK =>

										type_texts_with_content.append (
											container	=> packge.stop_mask.top.texts,
											new_item	=> pac_text);
										
									-- CS SEC_KEEPOUT
										
									when others => invalid_section;
								end case;

								-- clean up for next text
								pac_text := (others => <>);
								
							when SEC_BOTTOM =>
								case stack.parent (degree => 2) is
									when SEC_COPPER =>
										
										type_texts_with_content.append (
											container	=> packge.copper.bottom.texts,
											new_item	=> pac_text);

									when SEC_SILK_SCREEN =>

										type_texts_with_content.append (
											container	=> packge.silk_screen.bottom.texts,
											new_item	=> pac_text);


									when SEC_ASSEMBLY_DOCUMENTATION =>

										type_texts_with_content.append (
											container	=> packge.assembly_documentation.bottom.texts,
											new_item	=> pac_text);
										
									when SEC_STOP_MASK =>

										type_texts_with_content.append (
											container	=> packge.stop_mask.bottom.texts,
											new_item	=> pac_text);
										
									-- CS SEC_KEEPOUT
										
									when others => invalid_section;
								end case;
								
								-- clean up for next text
								pac_text := (others => <>);

							when others => invalid_section;
						end case;

					when SEC_PLACEHOLDER =>
						case stack.parent is
							when SEC_TOP =>
								case stack.parent (degree => 2) is
									when SEC_SILK_SCREEN =>
										
										et_packages.pac_text_placeholders.append (
											container	=> packge.silk_screen.top.placeholders,
											new_item	=> pac_text_placeholder);

									when SEC_ASSEMBLY_DOCUMENTATION =>
										
										et_packages.pac_text_placeholders.append (
											container	=> packge.assembly_documentation.top.placeholders,
											new_item	=> pac_text_placeholder);
										
									when others => invalid_section;
								end case;

								-- clean up for next placeholder
								pac_text_placeholder := (others => <>);

							when SEC_BOTTOM =>
								case stack.parent (degree => 2) is
									when SEC_SILK_SCREEN =>
										
										et_packages.pac_text_placeholders.append (
											container	=> packge.silk_screen.bottom.placeholders,
											new_item	=> pac_text_placeholder);

									when SEC_ASSEMBLY_DOCUMENTATION =>
										
										et_packages.pac_text_placeholders.append (
											container	=> packge.assembly_documentation.bottom.placeholders,
											new_item	=> pac_text_placeholder);
										
									when others => invalid_section;
								end case;

								-- clean up for next placeholder
								pac_text_placeholder := (others => <>);
								
							when others => invalid_section;
						end case;

					when SEC_TERMINAL =>
						case stack.parent is
							when SEC_TERMINALS => 
								-- Now all elements of the terminal have been read
								-- and can be assembled to the final terminal:
								build_terminal;
								
							when others => invalid_section;
						end case;

					when SEC_PAD_CONTOURS_SMT =>
						case stack.parent is
							when SEC_TERMINAL => 
								smt_pad_shape := (shapes.type_polygon_base (polygon) with null record);
								board_reset_polygon;
								
							when others => invalid_section;
						end case;

					when SEC_PAD_CONTOURS_THT =>
						case stack.parent is
							when SEC_TERMINAL => null;
							when others => invalid_section;
						end case;

					when SEC_MILLINGS =>
						case stack.parent is
							when SEC_TERMINAL => 
								tht_millings := (shapes.type_polygon_base (polygon) with null record);
								board_reset_polygon;
								
							when others => invalid_section;
						end case;
						
					when SEC_INIT => raise constraint_error;
						
				end case;

			end execute_section;

			function set (
			-- Tests if the current line is a section header or footer. Returns true in both cases.
			-- Returns false if the current line is neither a section header or footer.
			-- If it is a header, the section name is pushed onto the sections stack.
			-- If it is a footer, the latest section name is popped from the stack.
				section_keyword	: in string; -- [POLYGON
				section			: in type_section_name_package) -- SEC_FILL_ZONE
				return boolean is 
			begin -- set
				if f (line, 1) = section_keyword then -- section name detected in field 1
					if f (line, 2) = section_begin then -- section header detected in field 2
						stack.push (section);
						log (text => write_enter_section & to_string (section), level => log_threshold + 3);
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
							log (text => write_top_level_reached, level => log_threshold + 3);
						else
							log (text => write_return_to_section & to_string (stack.current), level => log_threshold + 3);
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
			if set (section_top, SEC_TOP) then null;			
			elsif set (section_bottom, SEC_BOTTOM) then null;								
			elsif set (section_line, SEC_LINE) then null;
			elsif set (section_arc, SEC_ARC) then null;
			elsif set (section_circle, SEC_CIRCLE) then null;
			elsif set (section_silk_screen, SEC_SILK_SCREEN) then null;
			elsif set (section_assembly_doc, SEC_ASSEMBLY_DOCUMENTATION) then null;
			elsif set (section_keepout, SEC_KEEPOUT) then null;			
			elsif set (section_copper, SEC_COPPER) then null;
			elsif set (section_stop_mask, SEC_STOP_MASK) then null;			
			elsif set (section_stencil, SEC_STENCIL) then null;			
			elsif set (section_route_restrict, SEC_ROUTE_RESTRICT) then null;			
			elsif set (section_via_restrict, SEC_VIA_RESTRICT) then null;
			elsif set (section_pcb_contours, SEC_PCB_CONTOURS_NON_PLATED) then null;
			elsif set (section_pad_contours_smt, SEC_PAD_CONTOURS_SMT) then null;
			elsif set (section_pad_contours_tht, SEC_PAD_CONTOURS_THT) then null;
			elsif set (section_pad_millings, SEC_MILLINGS) then null;			
			elsif set (section_text, SEC_TEXT) then null;
			elsif set (section_placeholder, SEC_PLACEHOLDER) then null;
			elsif set (section_terminals, SEC_TERMINALS) then null;
			elsif set (section_terminal, SEC_TERMINAL) then null;
			elsif set (section_fill_zone, SEC_FILL_ZONE) then null;
			elsif set (section_contours, SEC_CONTOURS) then null;
			elsif set (section_cutout_zone, SEC_CUTOUT_ZONE) then null;
			else
				-- The line contains something else -> the payload data. 
				-- Temporarily this data is stored in corresponding variables.

				log (text => "package line --> " & to_string (line), level => log_threshold + 3);
		
				case stack.current is

					when SEC_INIT =>
						declare
							kw : string := f (line, 1);
						begin
							-- CS: In the following: set a corresponding parameter-found-flag
							if kw = keyword_appearance then -- appearance real/virtual
								expect_field_count (line, 2);
								pac_appearance := to_appearance (f (line,2));

								-- Depending on the appearance we create a virtual or real package
								-- where pointer packge is pointing at:
								case pac_appearance is
									when REAL =>
										packge := new type_package' (
													appearance	=> REAL,
													others		=> <>);

									when VIRTUAL =>
										packge := new type_package' (
													appearance	=> VIRTUAL,
													others		=> <>);
								end case;
										
							elsif kw = keyword_description then -- description "blabla"
								expect_field_count (line, 2);
								pac_description := to_package_description (f (line,2));

							elsif kw = keyword_assembly_technology then -- technology SMT/THT
								expect_field_count (line, 2);
								pac_technology := to_assembly_technology (f (line,2));
								
							else
								invalid_keyword (kw);
							end if;
						end;

					when SEC_COPPER | SEC_KEEPOUT | SEC_STOP_MASK | SEC_STENCIL | 
						SEC_SILK_SCREEN | SEC_ASSEMBLY_DOCUMENTATION |
						SEC_ROUTE_RESTRICT | SEC_VIA_RESTRICT | SEC_PCB_CONTOURS_NON_PLATED | 
						SEC_TERMINALS | SEC_PACKAGE_3D_CONTOURS =>

						case stack.parent is
							when SEC_INIT => null;
							when others => invalid_section;
						end case;

					when SEC_TOP | SEC_BOTTOM =>
						case stack.parent is
							when SEC_COPPER | SEC_KEEPOUT | SEC_STOP_MASK | SEC_STENCIL | 
								SEC_SILK_SCREEN | SEC_ASSEMBLY_DOCUMENTATION |
								SEC_PAD_CONTOURS_THT => null;

							when others => invalid_section;
						end case;
						
					when SEC_LINE =>
						case stack.parent is
							when SEC_TOP | SEC_BOTTOM => 
								case stack.parent (degree => 2) is
									when SEC_COPPER | SEC_SILK_SCREEN | SEC_ASSEMBLY_DOCUMENTATION |
										SEC_STENCIL | SEC_STOP_MASK =>

										if not read_board_line (line) then
											declare
												kw : string := f (line, 1);
											begin
												-- CS: In the following: set a corresponding parameter-found-flag
												if kw = keyword_width then -- width 0.5
													expect_field_count (line, 2);
													board_line_width := to_distance (f (line, 2));
													
												else
													invalid_keyword (kw);
												end if;
											end;
										end if;
										
									when SEC_KEEPOUT => read_board_line (line);
										
									when SEC_PAD_CONTOURS_THT => read_board_line (line);

									when others => invalid_section;
								end case;

							when SEC_PCB_CONTOURS_NON_PLATED => read_board_line (line);

							when SEC_ROUTE_RESTRICT | SEC_VIA_RESTRICT =>
								if not read_board_line (line) then
									declare
										kw : string := f (line, 1);
										use et_pcb_stack;
									begin
										-- CS: In the following: set a corresponding parameter-found-flag
										if kw = keyword_layers then -- layers 2..16
											
											-- there must be at least two fields:
											expect_field_count (line => line, count_expected => 2, warn => false);
											signal_layers := to_layers (line);
											
										else
											invalid_keyword (kw);
										end if;
									end;
								end if;
								
							when SEC_PAD_CONTOURS_SMT => read_board_line (line);

							when SEC_MILLINGS => read_board_line (line);

							when SEC_CONTOURS => read_board_line (line);
								
							when others => invalid_section;
						end case;
						
					when SEC_ARC =>
						case stack.parent is
							when SEC_TOP | SEC_BOTTOM => 
								case stack.parent (degree => 2) is
									when SEC_COPPER | SEC_SILK_SCREEN | SEC_ASSEMBLY_DOCUMENTATION |
										SEC_STENCIL | SEC_STOP_MASK =>

										if not read_board_arc (line) then
											declare
												kw : string := f (line, 1);
											begin
												-- CS: In the following: set a corresponding parameter-found-flag
												if kw = keyword_width then -- width 0.5
													expect_field_count (line, 2);
													board_line_width := to_distance (f (line, 2));
													
												else
													invalid_keyword (kw);
												end if;
											end;
										end if;
										
									when SEC_KEEPOUT => read_board_arc (line);
										
									when SEC_PAD_CONTOURS_THT => read_board_arc (line);
										
									when others => invalid_section;
								end case;

							when SEC_PCB_CONTOURS_NON_PLATED => read_board_arc (line);

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
								
							when SEC_PAD_CONTOURS_SMT => read_board_arc (line);

							when SEC_MILLINGS => read_board_arc (line);

							when SEC_CONTOURS => read_board_arc (line);
								
							when others => invalid_section;
						end case;

					when SEC_CIRCLE =>
						case stack.parent is
							when SEC_TOP | SEC_BOTTOM => 
								case stack.parent (degree => 2) is
									when SEC_SILK_SCREEN | SEC_ASSEMBLY_DOCUMENTATION |
										SEC_STENCIL | SEC_STOP_MASK =>
										if not read_board_circle (line) then
											declare
												kw : string := f (line, 1);
											begin
												-- CS: In the following: set a corresponding parameter-found-flag
												if kw = keyword_width then -- width 0.5
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
													board_hatching.line_width := to_distance (f (line, 2));

												elsif kw = keyword_hatching_line_spacing then -- hatching_line_spacing 0.3
													expect_field_count (line, 2);													
													board_hatching.spacing := to_distance (f (line, 2));
													
												else
													invalid_keyword (kw);
												end if;
											end;
										end if;
										
									when SEC_KEEPOUT => read_board_circle (line);
										
									when SEC_COPPER => -- NON-ELECTRIC !!
										if not read_board_circle (line) then
											declare
												kw : string := f (line, 1);
											begin
												-- CS: In the following: set a corresponding parameter-found-flag
												if kw = keyword_width then -- width 0.5
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

												elsif kw = keyword_hatching_border_width then -- hatching_border_width 1.0
													board_hatching_copper.border_width := to_distance (f (line, 2));

												elsif kw = keyword_hatching_line_spacing then -- hatching_line_spacing 0.3
													expect_field_count (line, 2);													
													board_hatching_copper.spacing := to_distance (f (line, 2));
													
												else
													invalid_keyword (kw);
												end if;
											end;
										end if;
										
									when SEC_PAD_CONTOURS_THT => read_board_circle (line);
										
									when others => invalid_section;
								end case;

							when SEC_PCB_CONTOURS_NON_PLATED => read_board_circle (line);

							when SEC_ROUTE_RESTRICT | SEC_VIA_RESTRICT =>
								if not read_board_circle (line) then
									declare
										kw : string := f (line, 1);
										use et_pcb_stack;
									begin
										-- CS: In the following: set a corresponding parameter-found-flag
										if kw = keyword_filled then -- filled yes/no
											expect_field_count (line, 2);													
											board_filled := to_filled (f (line, 2));

										elsif kw = keyword_fill_style then -- fill_style solid/hatched
											expect_field_count (line, 2);													
											board_fill_style := to_fill_style (f (line, 2));

										elsif kw = keyword_hatching_line_width then -- hatching_line_width 0.3
											expect_field_count (line, 2);													
											board_hatching.line_width := to_distance (f (line, 2));

										elsif kw = keyword_hatching_line_spacing then -- hatching_line_spacing 0.3
											expect_field_count (line, 2);													
											board_hatching.spacing := to_distance (f (line, 2));

										elsif kw = keyword_layers then -- layers 1 14 3

											-- there must be at least two fields:
											expect_field_count (line => line, count_expected => 2, warn => false);

											signal_layers := to_layers (line);
											
										else
											invalid_keyword (kw);
										end if;
									end;
								end if;
								
							when SEC_PAD_CONTOURS_SMT => read_board_circle (line);

							when SEC_MILLINGS => read_board_circle (line);

							when SEC_CONTOURS => read_board_circle (line);
								
							when others => invalid_section;
						end case;

					when SEC_CUTOUT_ZONE =>
						case stack.parent is
							when SEC_TOP | SEC_BOTTOM => 
								case stack.parent (degree => 2) is
									when SEC_SILK_SCREEN | SEC_ASSEMBLY_DOCUMENTATION |
										SEC_STENCIL | SEC_STOP_MASK =>
										declare
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
										
									when SEC_COPPER =>
										declare
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

									when others => invalid_section;
								end case;
										
							when SEC_ROUTE_RESTRICT | SEC_VIA_RESTRICT =>
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

							when others => invalid_section;
						end case;
						
					when SEC_FILL_ZONE =>
						case stack.parent is
							when SEC_TOP | SEC_BOTTOM => 
								case stack.parent (degree => 2) is
									when SEC_SILK_SCREEN | SEC_ASSEMBLY_DOCUMENTATION |
										SEC_STENCIL | SEC_STOP_MASK =>
										declare
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
											kw : string := f (line, 1);
										begin
											if kw = keyword_filled then -- filled yes/no
												expect_field_count (line, 2);
												board_filled := to_filled (f (line, 2));
											else
												invalid_keyword (kw);
											end if;
										end;
										
									when SEC_COPPER =>
										declare
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

											elsif kw = keyword_isolation then -- isolation 0.5
												expect_field_count (line, 2);
												polygon_isolation := to_distance (f (line, 2));

											elsif kw = keyword_width then -- width 0.5
												expect_field_count (line, 2);
												polygon_width_min := to_distance (f (line, 2));
												
											else
												invalid_keyword (kw);
											end if;
										end;

									when others => invalid_section;
								end case;
										
							when SEC_ROUTE_RESTRICT | SEC_VIA_RESTRICT =>
								declare
									kw : string := f (line, 1);
									use et_pcb_stack;
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

							when others => invalid_section;
						end case;
						
					when SEC_CONTOURS =>
						case stack.parent is
							when SEC_FILL_ZONE => null;
							when SEC_CUTOUT_ZONE => null;
							when others => invalid_section;
						end case;

					when SEC_TEXT =>
						case stack.parent is
							when SEC_TOP | SEC_BOTTOM =>
								case stack.parent (degree => 2) is
									when SEC_COPPER | SEC_SILK_SCREEN | SEC_ASSEMBLY_DOCUMENTATION | SEC_STOP_MASK => -- CS SEC_KEEPOUT
										declare
											kw : string := f (line, 1);
										begin
											-- CS: In the following: set a corresponding parameter-found-flag
											if kw = keyword_position then -- position x 91.44 y 118.56 rotation 45.0
												expect_field_count (line, 7);

												-- extract position of note starting at field 2
												pac_text.position := to_position (line, 2);

											elsif kw = keyword_size then -- size width 1.000 height 1.000
												expect_field_count (line, 5);

												-- extract text dimensions starting at field 2
												pac_text.dimensions := to_dimensions (line, 2);

											elsif kw = keyword_line_width then -- line_width 0.1
												expect_field_count (line, 2);
												pac_text.line_width := to_distance (f (line, 2));

											elsif kw = keyword_alignment then -- alignment horizontal center vertical center
												expect_field_count (line, 5);

												-- extract alignment starting at field 2
												pac_text.alignment := to_alignment (line, 2);
												
											elsif kw = keyword_content then -- content "blabla"
												expect_field_count (line, 2); -- actual content in quotes !
												pac_text.content := et_libraries.to_content (f (line, 2));
												
											else
												invalid_keyword (kw);
											end if;
										end;
										
									when others => invalid_section;
								end case;

							when others => invalid_section;
								
						end case;

					when SEC_PLACEHOLDER =>
						case stack.parent is
							when SEC_TOP | SEC_BOTTOM =>
								case stack.parent (degree => 2) is
									when SEC_SILK_SCREEN | SEC_ASSEMBLY_DOCUMENTATION =>
										declare
											kw : string := f (line, 1);
										begin
											-- CS: In the following: set a corresponding parameter-found-flag
											if kw = keyword_position then -- position x 91.44 y 118.56 rotation 45.0
												expect_field_count (line, 7);

												-- extract position of note starting at field 2
												pac_text_placeholder.position := to_position (line, 2);

											elsif kw = keyword_size then -- size width 1.000 height 1.000
												expect_field_count (line, 5);

												-- extract text dimensions starting at field 2
												pac_text_placeholder.dimensions := to_dimensions (line, 2);

											elsif kw = keyword_line_width then -- line_width 0.1
												expect_field_count (line, 2);
												pac_text_placeholder.line_width := to_distance (f (line, 2));

											elsif kw = keyword_alignment then -- alignment horizontal center vertical center
												expect_field_count (line, 5);

												-- extract alignment starting at field 2
												pac_text_placeholder.alignment := to_alignment (line, 2);
												
											elsif kw = keyword_meaning then -- meaning reference, value, purpose
												expect_field_count (line, 2);
												pac_text_placeholder.meaning := to_text_meaning (f (line, 2));
												
											else
												invalid_keyword (kw);
											end if;
										end;

									when others => invalid_section;
								end case;

							when others => invalid_section;
						end case;

					when SEC_TERMINAL =>
						case stack.parent is
							when SEC_TERMINALS =>
								declare
									kw : string := f (line, 1);
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_name then -- name 1,2,H7
										expect_field_count (line, 2);
										terminal_name := et_libraries.to_terminal_name (f (line,2));

									elsif kw = keyword_assembly_technology then -- technology tht
										expect_field_count (line, 2);
										terminal_technology := to_assembly_technology (f (line,2));

									elsif kw = keyword_position then -- position x 12.7 y 3.0 rotation 0.0
										expect_field_count (line, 7);
										terminal_position := to_position (line,2);

									elsif kw = keyword_width_inner_layers then -- width_inner_layers 0.2
										expect_field_count (line, 2);
										tht_width_inner_layers := to_distance (f (line,2));

									elsif kw = keyword_tht_hole then -- hole drilled/milled
										expect_field_count (line, 2);
										tht_hole := to_tht_hole (f (line,2));

									elsif kw = keyword_drill_size then -- drill_size 0.8
										expect_field_count (line, 2);
										tht_drill_size := to_distance (f (line,2));
										
									elsif kw = et_pcb_coordinates.keyword_face then -- face top/bottom
										expect_field_count (line, 2);
										smt_pad_face := et_pcb_coordinates.to_face (f (line,2));

									elsif kw = keyword_stop_mask then -- stop_mask open/closed
										expect_field_count (line, 2);
										smt_stop_mask := to_stop_mask_status (f (line,2));

									elsif kw = keyword_solder_paste then -- solder_paste applied/none
										expect_field_count (line, 2);
										smt_solder_paste := to_solder_paste_status (f (line,2));
										
									else
										invalid_keyword (kw);
									end if;
								end;

							when others => invalid_section;
						end case;

					when SEC_PAD_CONTOURS_SMT | SEC_PAD_CONTOURS_THT | SEC_MILLINGS =>
						case stack.parent is
							when SEC_TERMINAL => null;
							when others => invalid_section;
						end case;
						
				end case;
			end if;

			exception when event: others =>
				log (text => "file " & et_libraries.to_string (file_name) & latin_1.space 
					 & affected_line (line) & to_string (line), console => true);
				raise;
			
		end process_line;
		
		previous_input : ada.text_io.file_type renames current_input;
		
	begin -- read_package
		log_indentation_up;
		log (text => "reading package " & et_libraries.to_string (file_name) & " ...", level => log_threshold);
		log_indentation_up;
		
		-- test if container et_pcb.packages already contains the package
		-- named "file_name". If so, there would be no need to read the file_name again.
		if type_packages.contains (packages, file_name) then
			log (text => "already read -> skipped", level => log_threshold + 1);
		else
			
			-- open package file
			open (
				file => file_handle,
				mode => in_file, 
				name => expand (et_libraries.to_string (file_name)));

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
					ifs 			=> latin_1.space); -- fields are separated by space

				-- we are interested in lines that contain something. emtpy lines are skipped:
				if field_count (line) > 0 then
					process_line;
				end if;
			end loop;

			-- As a safety measure the top section must be reached finally.
			if stack.depth > 1 then 
				log (WARNING, write_section_stack_not_empty);
			end if;

			set_input (previous_input);
			close (file_handle);

			-- Assign description and technology as they have been read earlier.
			packge.description := pac_description;
			packge.technology := pac_technology;

			-- Insert the package (accessed by pointer packge) in et_pcb.packages:
			type_packages.insert (
				container	=> packages, 
				key			=> file_name, -- libraries/packages/S_SO14.pac
				new_item	=> packge.all);

		end if;

		-- CS Check integrity of package (style guides, conventions ...)
		-- use function "last" to fetch latest package

		log_indentation_down;
		log_indentation_down;		

		exception when event: others =>
			if is_open (file_handle) then 
				set_input (previous_input);
				close (file_handle); 
			end if;
			raise;

	end read_package;
	
	procedure read_symbol (
	-- Opens the symbol file and stores the symbol in container et_libraries.symbols.
		file_name 		: in et_libraries.type_symbol_model_file.bounded_string; -- libraries/symbols/nand.sym
		log_threshold	: in et_string_processing.type_log_level) is
		use et_coordinates.geometry;
		use et_string_processing;
		use et_libraries;
		file_handle : ada.text_io.file_type;

		line : et_string_processing.type_fields_of_line;

		-- This is the section stack of the symbol model. 
		-- Here we track the sections. On entering a section, its name is
		-- pushed onto the stack. When leaving a section the latest section name is popped.
		max_section_depth : constant positive := 3; -- incl. section init
		package stack is new stack_lifo (
			item	=> type_section_name_symbol,
			max 	=> max_section_depth);

		function to_string (section : in type_section_name_symbol) return string is
		-- Converts a section like SEC_DRAW to a string "draw".
			len : positive := type_section_name_symbol'image (section)'length;
		begin
			return to_lower (type_section_name_symbol'image (section) (5..len));
		end to_string;
		
		-- VARIABLES FOR TEMPORARILY STORAGE AND ASSOCIATED HOUSEKEEPING SUBPROGRAMS:
		appearance			: type_device_appearance; -- sch, sch_pcb
		symbol				: access type_symbol;
		symbol_line			: et_libraries.type_line;
		symbol_arc			: et_libraries.type_arc;
		symbol_circle		: et_libraries.type_circle;
		symbol_text_base	: et_libraries.type_text_basic;
		symbol_text_position: et_coordinates.geometry.type_point;
		symbol_text_content	: et_libraries.type_text_content.bounded_string;
		symbol_placeholder_meaning : et_libraries.type_text_meaning := text_meaning_default;
		
		port					: et_libraries.type_port_base;
		port_name				: et_libraries.type_port_name.bounded_string;
		port_direction			: et_libraries.type_port_direction := port_direction_default;
		port_sensitivity_edge	: et_libraries.type_sensitivity_edge := sensitivity_edge_default;
		port_sensitivity_level	: et_libraries.type_sensitivity_level := sensitivity_level_default;
		port_output_inverted	: et_libraries.type_output_inverted := output_inverted_default;
		port_output_tristate	: et_libraries.type_output_tristate := output_tristate_default;
		port_output_weakness	: et_libraries.type_output_weakness := output_weakness_default;
		port_power_level		: et_libraries.type_power_level := port_power_level_default;

		procedure insert_port is 
			inserted	: boolean;
			cursor		: type_ports.cursor;
		begin
			case port_direction is
				when PASSIVE =>
					type_ports.insert (
						container	=> symbol.ports,
						key			=> port_name,
						inserted	=> inserted,
						position	=> cursor,
						new_item	=> (port with 
							direction				=> PASSIVE)
						);

				when INPUT_ANALOG =>
					type_ports.insert (
						container	=> symbol.ports,
						key			=> port_name,
						inserted	=> inserted,
						position	=> cursor,
						new_item	=> (port with 
							direction				=> INPUT_ANALOG)
						);

				when INPUT_DIGITAL =>
					type_ports.insert (
						container	=> symbol.ports,
						key			=> port_name,
						inserted	=> inserted,
						position	=> cursor,
						new_item	=> (port with 
							direction				=> INPUT_DIGITAL,
							sensitivity_edge		=> port_sensitivity_edge,
							sensitivity_level		=> port_sensitivity_level)
						);

				when OUTPUT_ANALOG =>
					type_ports.insert (
						container	=> symbol.ports,
						key			=> port_name,
						inserted	=> inserted,
						position	=> cursor,
						new_item	=> (port with 
							direction				=> OUTPUT_ANALOG,
							output_analog_tristate	=> port_output_tristate,
							output_analog_weakness	=> port_output_weakness)
						);

				when OUTPUT_DIGITAL =>
					type_ports.insert (
						container	=> symbol.ports,
						key			=> port_name,
						inserted	=> inserted,
						position	=> cursor,
						new_item	=> (port with 
							direction				=> OUTPUT_DIGITAL,
							output_digital_inverted	=> port_output_inverted,
							output_digital_tristate	=> port_output_tristate,
							output_digital_weakness	=> port_output_weakness)
						);

				when BIDIR_DIGITAL =>
					type_ports.insert (
						container	=> symbol.ports,
						key			=> port_name,
						inserted	=> inserted,
						position	=> cursor,
						new_item	=> (port with 
							direction				=> BIDIR_DIGITAL,
							output_inverted			=> port_output_inverted,
							output_tristate			=> port_output_tristate,
							output_weakness			=> port_output_weakness,
							input_sensitivity_edge	=> port_sensitivity_edge,
							input_sensitivity_level	=> port_sensitivity_level)
						);

				when POWER_OUT =>
					type_ports.insert (
						container	=> symbol.ports,
						key			=> port_name,
						inserted	=> inserted,
						position	=> cursor,
						new_item	=> (port with 
							direction				=> POWER_OUT,
							level					=> port_power_level)
						);

				when POWER_IN =>
					type_ports.insert (
						container	=> symbol.ports,
						key			=> port_name,
						inserted	=> inserted,
						position	=> cursor,
						new_item	=> (port with 
							direction				=> POWER_IN,
							level					=> port_power_level)
						);

				when NOT_CONNECTED =>
					type_ports.insert (
						container	=> symbol.ports,
						key			=> port_name,
						inserted	=> inserted,
						position	=> cursor,
						new_item	=> (port with 
							direction				=> NOT_CONNECTED)
						);
			end case;

			-- abort if port name already used:
			if not inserted then
				log (ERROR, "port " & to_string (port_name) & " already in use !", console => true);
				raise constraint_error;
			end if;
			
			-- reset port parameters for next port
			port					:= (others => <>);
			port_name				:= to_port_name ("");
			port_direction			:= port_direction_default;
			port_sensitivity_edge	:= sensitivity_edge_default;
			port_sensitivity_level	:= sensitivity_level_default;
			port_output_inverted	:= output_inverted_default;
			port_output_tristate	:= output_tristate_default;
			port_output_weakness	:= output_weakness_default;
			port_power_level		:= port_power_level_default;
		end insert_port;
		
		procedure process_line is 

			procedure execute_section is
			-- Once a section concludes, the temporarily variables are read, evaluated
			-- and finally assembled to actual objects:
			begin -- execute_section
				case stack.current is

					when SEC_DRAW | SEC_TEXTS | SEC_PLACEHOLDERS | SEC_PORTS => 
						case stack.parent is
							when SEC_INIT => null;
							when others => invalid_section;
						end case;

					when SEC_LINE =>
						case stack.parent is
							when SEC_DRAW => 

								-- append symbol_line to unit_symbol
								et_libraries.type_lines.append (
									container	=> symbol.shapes.lines,
									new_item	=> symbol_line);

								-- clean up for next line
								symbol_line := (others => <>);
								
							when others => invalid_section;
						end case;

					when SEC_ARC =>
						case stack.parent is
							when SEC_DRAW =>

								-- append symbol_arc to unit_symbol
								et_libraries.type_arcs.append (
									container	=> symbol.shapes.arcs,
									new_item	=> symbol_arc);

								-- clean up for next arc
								symbol_arc := (others => <>);
								
							when others => invalid_section;
						end case;
						
					when SEC_CIRCLE =>
						case stack.parent is
							when SEC_DRAW =>

								-- append symbol_circle to unit_symbol
								et_libraries.type_circles.append (
									container	=> symbol.shapes.circles,
									new_item	=> symbol_circle);

								-- clean up for next circle
								symbol_circle := (others => <>);
								
							when others => invalid_section;
						end case;
						
					when SEC_TEXT =>
						case stack.parent is
							when SEC_TEXTS =>

								-- append symbol text to symbol
								type_symbol_texts.append (
									container	=> symbol.texts,
									new_item	=> (symbol_text_base with
											meaning		=> MISC,
											content		=> symbol_text_content,
											position	=> symbol_text_position));

								-- clean up for next symbol text
								symbol_text_base := (others => <>);
								symbol_text_content := to_content ("");
								symbol_text_position := geometry.origin;
								
							when others => invalid_section;
						end case;
						
					when SEC_PLACEHOLDER =>
						case stack.parent is
							when SEC_PLACEHOLDERS =>

								-- Assign symbol text placeholder to symbol.
								-- The meaning of the placeholder determines where
								-- the placeholder is to be assigned. 
								-- If meaning is not specified in section PLACEHOLDER,
								-- the default meaning is assumed which raise an error.

								-- CS: warn if placeholder exists multiple times. The latest
								-- placeholder would overwrite the previous one.

								case symbol_placeholder_meaning is
									when NAME =>
										symbol.name := (symbol_text_base with 
											position	=> symbol_text_position,
											meaning		=> symbol_placeholder_meaning);

									when et_libraries.VALUE =>
										symbol.value := (symbol_text_base with 
											position	=> symbol_text_position,
											meaning		=> symbol_placeholder_meaning);

									when PURPOSE =>
										symbol.purpose := (symbol_text_base with 
											position	=> symbol_text_position,
											meaning		=> symbol_placeholder_meaning);

									-- Default meaning causes an error:
									when others => 
										log (ERROR, "meaning of placeholder not specified !",
											 console => true);
										raise constraint_error;
								end case;

								-- clean up for next symbol text placeholder
								symbol_text_base := (others => <>);
								symbol_text_position := geometry.origin;
								symbol_placeholder_meaning := text_meaning_default;
							
							when others => invalid_section;
						end case;

					when SEC_PORT =>
						case stack.parent is
							when SEC_PORTS => insert_port;
							when others => invalid_section;
						end case;
						
					when SEC_INIT => null; -- CS: should never happen
				end case;

			end execute_section;

			function set (
			-- Tests if the current line is a section header or footer. Returns true in both cases.
			-- Returns false if the current line is neither a section header or footer.
			-- If it is a header, the section name is pushed onto the sections stack.
			-- If it is a footer, the latest section name is popped from the stack.
				section_keyword	: in string; -- [DRAW
				section			: in type_section_name_symbol) -- SEC_DRAW
				return boolean is 
			begin -- set
				if f (line, 1) = section_keyword then -- section name detected in field 1
					if f (line, 2) = section_begin then -- section header detected in field 2
						stack.push (section);
						log (text => write_enter_section & to_string (section), level => log_threshold + 3);
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
							log (text => write_top_level_reached, level => log_threshold + 3);
						else
							log (text => write_return_to_section & to_string (stack.current), level => log_threshold + 3);
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
			if set (section_draw, SEC_DRAW) then null;			
			elsif set (section_line, SEC_LINE) then null;								
			elsif set (section_arc, SEC_ARC) then null;								
			elsif set (section_circle, SEC_CIRCLE) then null;
			elsif set (section_texts, SEC_TEXTS) then null;
			elsif set (section_text, SEC_TEXT) then null;
			elsif set (section_placeholders, SEC_PLACEHOLDERS) then null;
			elsif set (section_placeholder, SEC_PLACEHOLDER) then null;
			elsif set (section_ports, SEC_PORTS) then null;
			elsif set (section_port, SEC_PORT) then null;
			else
				-- The line contains something else -> the payload data. 
				-- Temporarily this data is stored in corresponding variables.

				log (text => "symbol line --> " & to_string (line), level => log_threshold + 3);
		
				case stack.current is

					when SEC_INIT =>
						declare
							kw : string := f (line, 1);
						begin
							-- CS: In the following: set a corresponding parameter-found-flag
							if kw = keyword_appearance then -- appearance sch_pcb
								expect_field_count (line, 2);
								appearance := et_libraries.to_appearance (f (line,2));
-- 								log (text => "appearance" & to_string (appearance), level => log_threshold + 1);								

								-- Create a new symbol where pointer "symbol" is pointing at.
								case appearance is
									when et_libraries.SCH =>
										symbol := new et_libraries.type_symbol' (
											appearance	=> et_libraries.SCH,
											others		=> <>);

									when et_libraries.SCH_PCB =>
										symbol := new et_libraries.type_symbol' (
											appearance	=> et_libraries.SCH_PCB,
											others		=> <>);

									when others => 
										raise constraint_error; -- CS

								end case;
								
							else
								invalid_keyword (kw);
							end if;
						end;

					when SEC_DRAW | SEC_TEXTS | SEC_PLACEHOLDERS | SEC_PORTS => 
						case stack.parent is
							when SEC_INIT => null;
							when others => invalid_section;
						end case;

					when SEC_LINE =>
						case stack.parent is
							when SEC_DRAW =>
								declare
									kw : string := f (line, 1);
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_start then -- start x 1 y 2
										expect_field_count (line, 5);

										-- extract the start position starting at field 2
										symbol_line.start_point := to_position (line,2);
										
									elsif kw = keyword_end then -- end x 0.00 y 0.00
										expect_field_count (line, 5);

										-- extract the end position starting at field 2
										symbol_line.end_point := to_position (line,2);

									elsif kw = keyword_width then
										expect_field_count (line, 2);
										symbol_line.width := to_distance (f (line, 2));
										
									else
										invalid_keyword (kw);
									end if;
								end;

							when others => invalid_section;
						end case;

					when SEC_ARC =>
						case stack.parent is
							when SEC_DRAW =>
								declare
									kw : string := f (line, 1);
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = shapes.keyword_center then -- center x 1 y 2
										expect_field_count (line, 5);

										-- extract the start position starting at field 2
										symbol_arc.center := to_position (line,2);

									elsif kw = keyword_start then -- start x 1 y 2
										expect_field_count (line, 5);

										-- extract the start position starting at field 2
										symbol_arc.start_point := to_position (line,2);
										
									elsif kw = keyword_end then -- end x 0.00 y 0.00
										expect_field_count (line, 5);

										-- extract the end position starting at field 2
										symbol_arc.end_point := to_position (line,2);

									elsif kw = keyword_width then
										expect_field_count (line, 2);
										symbol_arc.width := to_distance (f (line, 2));

									elsif kw = shapes.keyword_radius then
										expect_field_count (line, 2);
										symbol_arc.radius := to_distance (f (line, 2));
										
									else
										invalid_keyword (kw);
									end if;
								end;

							when others => invalid_section;
						end case;
						
					when SEC_CIRCLE =>
						case stack.parent is
							when SEC_DRAW =>
								declare
									kw : string := f (line, 1);
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = shapes.keyword_center then -- center x 1 y 2
										expect_field_count (line, 5);

										-- extract the start position starting at field 2
										symbol_circle.center := to_position (line,2);

									elsif kw = keyword_width then -- widht 0.2
										expect_field_count (line, 2);
										symbol_circle.width := to_distance (f (line, 2));

									elsif kw = shapes.keyword_radius then -- radius 5
										expect_field_count (line, 2);
										symbol_circle.radius := to_distance (f (line, 2));

									elsif kw = et_libraries.shapes.keyword_filled then -- filled yes/no
										expect_field_count (line, 2);
										symbol_circle.filled := et_libraries.to_circle_filled (f (line, 2));
										
									else
										invalid_keyword (kw);
									end if;
								end;

							when others => invalid_section;
						end case;
						
					when SEC_TEXT =>
						case stack.parent is
							when SEC_TEXTS =>
								declare
									kw : string := f (line, 1);
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_position then -- position x 1 y 2
										expect_field_count (line, 5);

										-- extract the text position starting at field 2
										symbol_text_position := to_position (line,2);

									elsif kw = keyword_content then -- content "dummy NAND gate"
										expect_field_count (line, 2);
										symbol_text_content := et_libraries.to_content (f (line, 2));

									elsif kw = keyword_size then -- size 5
										expect_field_count (line, 2);
										symbol_text_base.size := to_distance (f (line, 2));

									elsif kw = keyword_line_width then -- line_width 0.2
										expect_field_count (line, 2);
										symbol_text_base.line_width := to_distance (f (line, 2));

									elsif kw = keyword_rotation then -- rotation 90.0
										expect_field_count (line, 2);
										symbol_text_base.rotation := to_rotation (f (line, 2));
										
									elsif kw = keyword_style then -- style italic
										expect_field_count (line, 2);
										symbol_text_base.style := et_libraries.to_text_style (f (line, 2));

									elsif kw = keyword_alignment then -- alignment horizontal center vertical center
										expect_field_count (line, 5);
										symbol_text_base.alignment := to_alignment (line, 2);

									else
										invalid_keyword (kw);
									end if;
								end;

							when others => invalid_section;
						end case;
						
					when SEC_PLACEHOLDER =>
						case stack.parent is
							when SEC_PLACEHOLDERS =>
								declare
									kw : string := f (line, 1);
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_position then -- position x 1 y 2
										expect_field_count (line, 5);

										-- extract the placeholder position starting at field 2
										symbol_text_position := to_position (line,2);

									elsif kw = keyword_meaning then -- meaning reference
										expect_field_count (line, 2);
										symbol_placeholder_meaning := et_libraries.to_text_meaning (f (line, 2));

									elsif kw = keyword_size then -- size 5
										expect_field_count (line, 2);
										symbol_text_base.size := to_distance (f (line, 2));

									elsif kw = keyword_line_width then -- line_width 0.2
										expect_field_count (line, 2);
										symbol_text_base.line_width := to_distance (f (line, 2));

									elsif kw = keyword_rotation then -- rotation 90.0
										expect_field_count (line, 2);
										symbol_text_base.rotation := to_rotation (f (line, 2));
										
									elsif kw = keyword_style then -- style italic
										expect_field_count (line, 2);
										symbol_text_base.style := et_libraries.to_text_style (f (line, 2));

									elsif kw = keyword_alignment then -- alignment horizontal center vertical center
										expect_field_count (line, 5);
										symbol_text_base.alignment := to_alignment (line, 2);

									else
										invalid_keyword (kw);
									end if;
								end;

							when others => invalid_section;
						end case;

					when SEC_PORT =>
						case stack.parent is
							when SEC_PORTS =>
								declare
									kw : string := f (line, 1);
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_position then -- position x 1 y 2
										expect_field_count (line, 5);

										-- extract the port position starting at field 2
										port.position := to_position (line,2);

									elsif kw = keyword_name then -- name I1A
										expect_field_count (line, 2);
										port_name := et_libraries.to_port_name (f (line, 2));

									elsif kw = keyword_length then -- length 5
										expect_field_count (line, 2);
										port.length := to_distance (f (line, 2));

									elsif kw = keyword_rotation then -- rotation 90.0
										expect_field_count (line, 2);
										port.rotation := to_rotation (f (line, 2));
										
									elsif kw = keyword_port_name_visible then -- port_name_visible yes/no
										expect_field_count (line, 2);
										port.port_name_visible := et_libraries.to_port_name_visible (f (line, 2));

									elsif kw = keyword_port_name_size then -- port_name_size 2.0
										expect_field_count (line, 2);
										port.port_name_size := to_distance (f (line, 2));

									elsif kw = keyword_terminal_name_visible then -- terminal_name_visible yes/no
										expect_field_count (line, 2);
										port.terminal_name_visible := et_libraries.to_terminal_name_visible (f (line, 2));

									elsif kw = keyword_terminal_name_size then -- terminal_name_size 2.0
										expect_field_count (line, 2);
										port.terminal_name_size := to_distance (f (line, 2));

									elsif kw = keyword_direction then -- direction BIDIR, PASSIVE, NOT_CONNECTED, ...
										expect_field_count (line, 2);
										port_direction := et_libraries.to_port_direction (f (line, 2));

									elsif kw = keyword_sensitivity_edge then -- sensitivity_edge rising/falling/any
										expect_field_count (line, 2);
										port_sensitivity_edge := et_libraries.to_sensitivity_edge (f (line, 2));

									elsif kw = keyword_sensitivity_level then -- sensitivity_level high/low
										expect_field_count (line, 2);
										port_sensitivity_level := et_libraries.to_sensitivity_level (f (line, 2));

									elsif kw = keyword_inverted then -- inverted yes/no
										expect_field_count (line, 2);
										port_output_inverted := et_libraries.to_output_inverted (f (line, 2));

									elsif kw = keyword_tristate then -- tristate yes/no
										expect_field_count (line, 2);
										port_output_tristate := et_libraries.to_output_tristate (f (line, 2));

									elsif kw = keyword_level then -- level positive/negative/zero
										expect_field_count (line, 2);
										port_power_level := et_libraries.to_power_level (f (line, 2));

									elsif kw = keyword_weakness then -- weakness none/pull0/weak1 ...
										expect_field_count (line, 2);
										port_output_weakness := et_libraries.to_output_weakness (f (line, 2));
										
									else
										invalid_keyword (kw);
									end if;
								end;

							when others => invalid_section;
						end case;
						
				end case;
			end if;

			exception when event: others =>
				log (text => "file " & to_string (file_name) & latin_1.space 
					 & affected_line (line) & to_string (line), console => true);
				raise;
			
		end process_line;

		previous_input : ada.text_io.file_type renames current_input;
		
	begin -- read_symbol
		log_indentation_up;
		log (text => "reading symbol " & to_string (file_name) & " ...", level => log_threshold);
		log_indentation_up;
		
		-- test if container et_libraries.symbols already contains the symbol
		-- named "file_name". If so, there would be no need to read the file_name again.
		if et_libraries.type_symbols.contains (et_libraries.symbols, file_name) then
			log (text => "already read -> skipped", level => log_threshold + 1);
		else
			
			-- open symbol file
			open (
				file => file_handle,
				mode => in_file, 
				name => expand (to_string (file_name)));

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
					ifs 			=> latin_1.space); -- fields are separated by space

				-- we are interested in lines that contain something. emtpy lines are skipped:
				if field_count (line) > 0 then
					process_line;
				end if;
			end loop;

			-- As a safety measure the top section must be reached finally.
			if stack.depth > 1 then 
				log (WARNING, write_section_stack_not_empty);
			end if;

			set_input (previous_input);
			close (file_handle);

			-- Insert the symbol (accessed by pointer symbol) in et_libraries.symbols:
			et_libraries.type_symbols.insert (
				container	=> et_libraries.symbols, 
				key			=> file_name, -- libraries/symbols/nand.sym
				new_item	=> symbol.all);

		end if;

		-- CS Check integrity of symbol (style guides, conventions ...)
		-- use function "last" to fetch latest symbol

		log_indentation_down;
		log_indentation_down;		

		exception when event: others =>
			if is_open (file_handle) then 
				set_input (previous_input);
				close (file_handle); 
			end if;
			raise;

	end read_symbol;

	
	procedure read_device_file (
	-- Opens the device and stores it in container et_libraries.devices.
		file_name 		: in et_libraries.type_device_model_file.bounded_string; -- ../lbr/logic_ttl/7400.dev
		log_threshold	: in et_string_processing.type_log_level) is
		use et_string_processing;
		use et_libraries;
		use geometry;
		file_handle : ada.text_io.file_type;

		line : et_string_processing.type_fields_of_line;

		-- This is the section stack of the device model. 
		-- Here we track the sections. On entering a section, its name is
		-- pushed onto the stack. When leaving a section the latest section name is popped.
		max_section_depth : constant positive := 6;
		package stack is new stack_lifo (
			item	=> type_section_name_device,
			max 	=> max_section_depth);

		function to_string (section : in type_section_name_device) return string is
		-- Converts a section like SEC_VARIANT to a string "variant".
			len : positive := type_section_name_device'image (section)'length;
		begin
			return to_lower (type_section_name_device'image (section) (5..len));
		end to_string;
		
		-- VARIABLES FOR TEMPORARILY STORAGE AND ASSOCIATED HOUSEKEEPING SUBPROGRAMS:
		prefix				: type_device_name_prefix.bounded_string; -- T, IC
		value				: type_value.bounded_string; -- BC548
		appearance			: type_device_appearance; -- sch, sch_pcb
		partcode			: material.type_partcode.bounded_string; -- IC_PAC_S_SOT23_VAL_
		variant				: type_component_variant;
		variant_name		: type_component_variant_name.bounded_string; -- N, D
		variants			: type_component_variants.map;
		terminal_port_map	: type_terminal_port_map.map;

		procedure insert_terminal (line : in type_fields_of_line) is -- terminal 14 unit 5 VCC
			use et_libraries;
			use type_terminal_port_map;
			inserted	: boolean;
			position	: type_terminal_port_map.cursor;

			terminal	: type_terminal_name.bounded_string; -- H5, 14
			unit		: type_unit_name.bounded_string; -- PWR, IO_BANK_2
			port		: type_port_name.bounded_string; -- VCC

			place : positive := 1; -- the field being read from given line

			-- CS: detect missing parameters
			-- CS: warn about wrong misplaced keywords
		begin
			while place <= positive (field_count (line)) loop
			
				-- We expect the terminal name after the keyword "terminal"
				if f (line, place) = keyword_terminal then
					terminal := to_terminal_name (f (line, place + 1)); -- 14

				-- After the keyword "unit" must come the unit name:
				elsif f (line, place) = keyword_unit then 
					unit := to_unit_name (f (line, place + 1)); -- 5

				-- After the keyword "port" must come the port name
				elsif f (line, place) = keyword_port then 
					port := to_port_name (f (line, place + 1)); -- VCC
					
				else
					invalid_keyword (f (line, place));
				end if;
					
				place := place + 2;
			end loop;

			-- insert terminal to port assigment in temporarily terminal_port_map
			insert (
				container	=> terminal_port_map,
				key			=> terminal, -- H5, 14
				inserted	=> inserted,
				position	=> position,
				new_item	=> (
								unit	=> unit, -- IO_BANK_2,
								name	=> port -- VCC
								));

			-- an assigment must be unique !
			if not inserted then
				log (ERROR, "terminal-to-port assigment already used !", console => true);
				raise constraint_error;
			end if;

			-- clean up for next terminal to port assigment
			terminal	:= to_terminal_name ("");
			unit		:= to_unit_name ("");
			port		:= to_port_name ("");
		end insert_terminal;

		procedure insert_variant is
			use type_component_variants;
			inserted : boolean;
			position : type_component_variants.cursor;
		begin
			et_libraries.check_variant_name_characters (variant_name);

			insert (
				container	=> variants,
				key			=> variant_name, -- N, D 
				inserted	=> inserted,
				position	=> position,
				new_item	=> variant);

			-- A particular variant must occur only once in the device model:
			if not inserted then
				log (ERROR, "variant " & to_string (variant_name) & " already used !", console => true);
				raise constraint_error;
			end if;

			-- read package model (like libraries/packages/__#__#lbr#bel_ic_pretty#S_SO14.pac)
			read_package (variant.package_model, log_threshold + 1);

			-- clean up for next variant
			variant := (others => <>);
		end insert_variant;

		unit_name			: type_unit_name.bounded_string; -- IO_BANK_2
		unit_position		: type_point := origin; -- the position of the unit inside the device editor
		unit_swap_level		: type_unit_swap_level := unit_swap_level_default;
		unit_add_level		: type_unit_add_level := unit_add_level_default;
		unit_symbol			: access type_symbol;
		units_internal		: type_units_internal.map;
		units_external		: type_units_external.map;
		symbol_line			: et_libraries.type_line;
		symbol_arc			: et_libraries.type_arc;
		symbol_circle		: et_libraries.type_circle;
		symbol_text_base	: et_libraries.type_text_basic;
		symbol_text_position: et_coordinates.geometry.type_point;
		symbol_text_content	: et_libraries.type_text_content.bounded_string;
		symbol_placeholder_meaning : et_libraries.type_text_meaning := text_meaning_default;
		
		port					: et_libraries.type_port_base;
		port_name				: et_libraries.type_port_name.bounded_string;
		port_direction			: et_libraries.type_port_direction := port_direction_default;
		port_sensitivity_edge	: et_libraries.type_sensitivity_edge := sensitivity_edge_default;
		port_sensitivity_level	: et_libraries.type_sensitivity_level := sensitivity_level_default;
		port_output_inverted	: et_libraries.type_output_inverted := output_inverted_default;
		port_output_tristate	: et_libraries.type_output_tristate := output_tristate_default;
		port_output_weakness	: et_libraries.type_output_weakness := output_weakness_default;
		port_power_level		: et_libraries.type_power_level := port_power_level_default;

		unit_external : type_unit_external;

		procedure insert_unit_internal is
		-- Inserts in the temporarily collection of internal units a new unit.
		-- The symbol of the unit is the one accessed by pointer unit_symbol.
			position : type_units_internal.cursor;
			inserted : boolean;
		begin
			-- Depending on the appearance of the device, a unit with the same
			-- appearance is inserted in units_internal.
			case appearance is 
				when SCH =>
					type_units_internal.insert (
						container	=> units_internal,
						position	=> position,
						inserted	=> inserted,
						key			=> unit_name,
						new_item	=> (
								appearance	=> SCH,
								symbol		=> unit_symbol.all,
								position	=> unit_position,
								swap_level	=> unit_swap_level,
								add_level	=> unit_add_level));

				when SCH_PCB =>
					type_units_internal.insert (
						container	=> units_internal,
						position	=> position,
						inserted	=> inserted,
						key			=> unit_name,
						new_item	=> (
								appearance	=> SCH_PCB,
								symbol		=> unit_symbol.all,
								position	=> unit_position,
								swap_level	=> unit_swap_level,
								add_level	=> unit_add_level));

				when others => null; -- CS
			end case;

			-- A unit name must occur only once. 
			-- Make sure the unit_name is not in use by any internal or external units:
			
			-- Test occurence in internal units:
			if not inserted then
				log (ERROR, "unit " & to_string (unit_name) 
					& " already used by another internal unit !", console => true);
				raise constraint_error;
			end if;

			-- Make sure the unit name is not in use by any external unit:
			if type_units_external.contains (units_external, unit_name) then
				log (ERROR, "unit name " & to_string (unit_name) 
					& " already used by an external unit !", console => true);
				raise constraint_error;
			end if;
			
			-- clean up for next unit
			unit_name := to_unit_name ("");
			unit_position := origin;
			unit_swap_level := unit_swap_level_default;
			unit_add_level := unit_add_level_default;
			unit_symbol := null;
			
		end insert_unit_internal;

		procedure insert_unit_external is
		-- Inserts in the temporarily collection of external units a new unit.
			position : type_units_external.cursor;
			inserted : boolean;
		begin
			type_units_external.insert (
				container	=> units_external,
				position	=> position,
				inserted	=> inserted,
				key			=> unit_name,
				new_item	=> unit_external);

			-- A unit name must occur only once. 
			-- Make sure the unit_name is not in use by any internal or external units:

			-- Test occurence in external units:
			if not inserted then
				log (ERROR, "unit name " & to_string (unit_name) 
					& " already used by another external unit !", console => true);
				raise constraint_error;
			end if;

			-- Make sure the unit name is not in use by any internal unit:
			if type_units_internal.contains (units_internal, unit_name) then
				log (ERROR, "unit name " & to_string (unit_name) 
					& " already used by an internal unit !", console => true);
				raise constraint_error;
			end if;

			-- read the symbol model (like ../libraries/symbols/power_gnd.sym)
			read_symbol (unit_external.file, log_threshold + 1);

			-- clean up for next unit
			unit_name := to_unit_name ("");
			unit_external := (others => <>);
		end insert_unit_external;

		procedure insert_port is 
			inserted	: boolean;
			cursor		: type_ports.cursor;
		begin
			case port_direction is
				when PASSIVE =>
					type_ports.insert (
						container	=> unit_symbol.ports,
						key			=> port_name,
						inserted	=> inserted,
						position	=> cursor,
						new_item	=> (port with 
							direction				=> PASSIVE)
						);

				when INPUT_ANALOG =>
					type_ports.insert (
						container	=> unit_symbol.ports,
						key			=> port_name,
						inserted	=> inserted,
						position	=> cursor,
						new_item	=> (port with 
							direction				=> INPUT_ANALOG)
						);

				when INPUT_DIGITAL =>
					type_ports.insert (
						container	=> unit_symbol.ports,
						key			=> port_name,
						inserted	=> inserted,
						position	=> cursor,
						new_item	=> (port with 
							direction				=> INPUT_DIGITAL,
							sensitivity_edge		=> port_sensitivity_edge,
							sensitivity_level		=> port_sensitivity_level)
						);

				when OUTPUT_ANALOG =>
					type_ports.insert (
						container	=> unit_symbol.ports,
						key			=> port_name,
						inserted	=> inserted,
						position	=> cursor,
						new_item	=> (port with 
							direction				=> OUTPUT_ANALOG,
							output_analog_tristate	=> port_output_tristate,
							output_analog_weakness	=> port_output_weakness)
						);

				when OUTPUT_DIGITAL =>
					type_ports.insert (
						container	=> unit_symbol.ports,
						key			=> port_name,
						inserted	=> inserted,
						position	=> cursor,
						new_item	=> (port with 
							direction				=> OUTPUT_DIGITAL,
							output_digital_inverted	=> port_output_inverted,
							output_digital_tristate	=> port_output_tristate,
							output_digital_weakness	=> port_output_weakness)
						);

				when BIDIR_DIGITAL =>
					type_ports.insert (
						container	=> unit_symbol.ports,
						key			=> port_name,
						inserted	=> inserted,
						position	=> cursor,
						new_item	=> (port with 
							direction				=> BIDIR_DIGITAL,
							output_inverted			=> port_output_inverted,
							output_tristate			=> port_output_tristate,
							output_weakness			=> port_output_weakness,
							input_sensitivity_edge	=> port_sensitivity_edge,
							input_sensitivity_level	=> port_sensitivity_level)
						);

				when POWER_OUT =>
					type_ports.insert (
						container	=> unit_symbol.ports,
						key			=> port_name,
						inserted	=> inserted,
						position	=> cursor,
						new_item	=> (port with 
							direction				=> POWER_OUT,
							level					=> port_power_level)
						);

				when POWER_IN =>
					type_ports.insert (
						container	=> unit_symbol.ports,
						key			=> port_name,
						inserted	=> inserted,
						position	=> cursor,
						new_item	=> (port with 
							direction				=> POWER_IN,
							level					=> port_power_level)
						);

				when NOT_CONNECTED =>
					type_ports.insert (
						container	=> unit_symbol.ports,
						key			=> port_name,
						inserted	=> inserted,
						position	=> cursor,
						new_item	=> (port with 
							direction				=> NOT_CONNECTED)
						);
			end case;

			-- abort if port name already used:
			if not inserted then
				log (ERROR, "port " & to_string (port_name) & " already in use !", console => true);
				raise constraint_error;
			end if;
			
			-- reset port parameters for next port
			port					:= (others => <>);
			port_name				:= to_port_name ("");
			port_direction			:= port_direction_default;
			port_sensitivity_edge	:= sensitivity_edge_default;
			port_sensitivity_level	:= sensitivity_level_default;
			port_output_inverted	:= output_inverted_default;
			port_output_tristate	:= output_tristate_default;
			port_output_weakness	:= output_weakness_default;
			port_power_level		:= port_power_level_default;

		end insert_port;
		
		procedure process_line is 

			procedure execute_section is
			-- Once a section concludes, the temporarily variables are read, evaluated
			-- and finally assembled to actual objects:
				
			begin -- execute_section
				case stack.current is

					when SEC_VARIANTS | SEC_UNITS_INTERNAL | SEC_UNITS_EXTERNAL => 
						case stack.parent is
							when SEC_INIT => null;
							when others => invalid_section;
						end case;

					when SEC_VARIANT =>
						case stack.parent is
							when SEC_VARIANTS =>
								-- insert the temporarily variant in the collection of variants
								insert_variant;

							when others => invalid_section;
						end case;

					when SEC_TERMINAL_PORT_MAP =>
						case stack.parent is
							when SEC_VARIANT =>
								-- copy temporarily terminal_port_map to current variant
								variant.terminal_port_map := terminal_port_map;

								-- clean up temporarily terminal_port_map for next variant
								et_libraries.type_terminal_port_map.clear (terminal_port_map);
							when others => invalid_section;
						end case;

					when SEC_UNIT =>
						case stack.parent is
							when SEC_UNITS_INTERNAL =>
								insert_unit_internal;
									
							when SEC_UNITS_EXTERNAL =>
								insert_unit_external;
								
							when others => invalid_section;
						end case;

					when SEC_SYMBOL =>
						case stack.parent is
							when SEC_UNIT => null; -- nothing to do
							when others => invalid_section;
						end case;

					when SEC_DRAW =>
						case stack.parent is
							when SEC_SYMBOL => null;  -- nothing to do
							when others => invalid_section;
						end case;
						
					when SEC_LINE =>
						case stack.parent is
							when SEC_DRAW => 

								-- append symbol_line to unit_symbol
								et_libraries.type_lines.append (
									container	=> unit_symbol.shapes.lines,
									new_item	=> symbol_line);

								-- clean up for next line
								symbol_line := (others => <>);
								
							when others => invalid_section;
						end case;

					when SEC_ARC =>
						case stack.parent is
							when SEC_DRAW =>

								-- append symbol_arc to unit_symbol
								et_libraries.type_arcs.append (
									container	=> unit_symbol.shapes.arcs,
									new_item	=> symbol_arc);

								-- clean up for next arc
								symbol_arc := (others => <>);
								
							when others => invalid_section;
						end case;
						
					when SEC_CIRCLE =>
						case stack.parent is
							when SEC_DRAW =>

								-- append symbol_circle to unit_symbol
								et_libraries.type_circles.append (
									container	=> unit_symbol.shapes.circles,
									new_item	=> symbol_circle);

								-- clean up for next circle
								symbol_circle := (others => <>);
								
							when others => invalid_section;
						end case;
						
					when SEC_TEXTS =>
						case stack.parent is
							when SEC_SYMBOL => null; -- nothing to do
							when others => invalid_section;
						end case;

					when SEC_TEXT =>
						case stack.parent is
							when SEC_TEXTS =>

								-- append symbol text to symbol
								type_symbol_texts.append (
									container	=> unit_symbol.texts,
									new_item	=> (symbol_text_base with
											meaning		=> MISC,
											content		=> symbol_text_content,
											position	=> symbol_text_position));

								-- clean up for next symbol text
								symbol_text_base := (others => <>);
								symbol_text_content := to_content ("");
								symbol_text_position := origin;
								
							when others => invalid_section;
						end case;
						
					when SEC_PLACEHOLDERS =>
						case stack.parent is
							when SEC_SYMBOL => null; -- nothing to do
							when others => invalid_section;
						end case;
						
					when SEC_PLACEHOLDER =>
						case stack.parent is
							when SEC_PLACEHOLDERS =>

								-- Assign symbol text placeholder to symbol.
								-- The meaning of the placeholder determines where
								-- the placeholder is to be assigned. 
								-- If meaning is not specified in section PLACEHOLDER,
								-- the default meaning is assumed which raise an error.

								-- CS: warn if placeholder exists multiple times. The latest
								-- placeholder would overwrite the previous one.

								case symbol_placeholder_meaning is
									when NAME =>
										unit_symbol.name := (symbol_text_base with 
											position	=> symbol_text_position,
											meaning		=> symbol_placeholder_meaning);

									when et_libraries.VALUE =>
										unit_symbol.value := (symbol_text_base with 
											position	=> symbol_text_position,
											meaning		=> symbol_placeholder_meaning);

									when PURPOSE =>
										unit_symbol.purpose := (symbol_text_base with 
											position	=> symbol_text_position,
											meaning		=> symbol_placeholder_meaning);

									-- Default meaning causes an error:
									when others => 
										log (ERROR, "meaning of placeholder not specified !",
											 console => true);
										raise constraint_error;
								end case;

								-- clean up for next symbol text placeholder
								symbol_text_base := (others => <>);
								symbol_text_position := origin;
								symbol_placeholder_meaning := text_meaning_default;
							
							when others => invalid_section;
						end case;

					when SEC_PORTS =>
						case stack.parent is 
							when SEC_SYMBOL => null; -- nothing to do
							when others => invalid_section;
						end case;

					when SEC_PORT =>
						case stack.parent is
							when SEC_PORTS => insert_port;
							when others => invalid_section;
						end case;
						
					when SEC_INIT => null; -- CS: should never happen
				end case;

			end execute_section;

			function set (
			-- Tests if the current line is a section header or footer. Returns true in both cases.
			-- Returns false if the current line is neither a section header or footer.
			-- If it is a header, the section name is pushed onto the sections stack.
			-- If it is a footer, the latest section name is popped from the stack.
				section_keyword	: in string; -- [UNIT
				section			: in type_section_name_device) -- SEC_UNIT
				return boolean is 
			begin -- set
				if f (line, 1) = section_keyword then -- section name detected in field 1
					if f (line, 2) = section_begin then -- section header detected in field 2
						stack.push (section);
						log (text => write_enter_section & to_string (section), level => log_threshold + 3);
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
							log (text => write_top_level_reached, level => log_threshold + 3);
						else
							log (text => write_return_to_section & to_string (stack.current), level => log_threshold + 3);
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
			if set (section_variants, SEC_VARIANTS) then null;
			elsif set (section_variant, SEC_VARIANT) then null;
			elsif set (section_terminal_port_map, SEC_TERMINAL_PORT_MAP) then null;
			elsif set (section_units_internal, SEC_UNITS_INTERNAL) then null;
			elsif set (section_units_external, SEC_UNITS_EXTERNAL) then null;			
			elsif set (section_unit, SEC_UNIT) then null;
			elsif set (section_symbol, SEC_SYMBOL) then null;
			elsif set (section_draw, SEC_DRAW) then null;			
			elsif set (section_line, SEC_LINE) then null;								
			elsif set (section_arc, SEC_ARC) then null;								
			elsif set (section_circle, SEC_CIRCLE) then null;
			elsif set (section_texts, SEC_TEXTS) then null;
			elsif set (section_text, SEC_TEXT) then null;
			elsif set (section_placeholders, SEC_PLACEHOLDERS) then null;
			elsif set (section_placeholder, SEC_PLACEHOLDER) then null;
			elsif set (section_ports, SEC_PORTS) then null;
			elsif set (section_port, SEC_PORT) then null;
			else
				-- The line contains something else -> the payload data. 
				-- Temporarily this data is stored in corresponding variables.

				log (text => "device line --> " & to_string (line), level => log_threshold + 3);
		
				case stack.current is

					when SEC_INIT =>
						declare
							kw : string := f (line, 1);
						begin
							-- CS: In the following: set a corresponding parameter-found-flag
							if kw = keyword_prefix then -- prefix IC
								expect_field_count (line, 2);
								et_libraries.check_prefix_length (f (line,2));
								prefix := et_libraries.to_prefix (f (line,2));
								et_libraries.check_prefix_characters (prefix);
								log (text => "prefix " & to_string (prefix), level => log_threshold + 1);
								
								if not conventions.prefix_valid (prefix) then
									--log (message_warning & "prefix of device model " &
									--	 to_string (file_name) & " not conformant with conventions !");
									null; -- CS output something helpful
								end if;

							elsif kw = keyword_value then -- value 7400
								expect_field_count (line, 2);

								-- validate value
								value := et_libraries.to_value (
										value						=> f (line, 2),
										error_on_invalid_character	=> false);

								if not et_libraries.value_characters_valid (value) then
									log (WARNING, "default value in device model " &
										 to_string (file_name) & " contains invalid characters !");
									log_indentation_reset;
									value_invalid (to_string (value));
								end if;
								
								log (text => "value " & to_string (value), level => log_threshold + 1);

							elsif kw = keyword_appearance then -- appearance sch_pcb
								expect_field_count (line, 2);
								appearance := et_libraries.to_appearance (f (line,2));
								log (text => "appearance" & to_string (appearance), level => log_threshold + 1);								

							elsif kw = keyword_partcode then -- partcode IC_PAC_S_SO14_VAL_
								expect_field_count (line, 2);

								-- validate partcode length
								partcode := material.to_partcode (f (line,2));
								
								log (text => "partcode " & material.to_string (partcode), level => log_threshold + 1);
							else
								invalid_keyword (kw);
							end if;
						end;

					when SEC_VARIANTS | SEC_UNITS_INTERNAL | SEC_UNITS_EXTERNAL => 
						case stack.parent is
							when SEC_INIT => null;
							when others => invalid_section;
						end case;

					when SEC_VARIANT =>
						case stack.parent is
							when SEC_VARIANTS =>
								declare
									kw : string := f (line, 1);
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_name then -- name D
										expect_field_count (line, 2);
										et_libraries.check_variant_name_length (f (line, 2));
										variant_name := et_libraries.to_component_variant_name (f (line,2));
										log (text => "variant " & to_string (variant_name), level => log_threshold + 1);
										
									elsif kw = keyword_package_model then -- package_model libraries/packages/S_SO14.pac
										expect_field_count (line, 2);

										-- The given path is something like libraries/packages/S_SO14.pac.
										-- Check if the package name like S_SO14 is too long or contains invalid characters.
										check_package_name_length (ada.directories.base_name (f (line, 2)));
										check_package_name_characters (to_package_name (ada.directories.base_name (f (line, 2))));

										variant.package_model := et_libraries.to_file_name (f (line,2));
										log (text => "package model " & to_string (variant.package_model), level => log_threshold + 1);
										
									else
										invalid_keyword (kw);
									end if;
								end;

							when others => invalid_section;
						end case;

					when SEC_TERMINAL_PORT_MAP =>
						case stack.parent is
							when SEC_VARIANT =>
								expect_field_count (line, 6); -- terminal 14 unit 5 port VCC

								-- extract terminal to port assignment
								insert_terminal (line);
							
							when others => invalid_section;
						end case;

					when SEC_UNIT =>
						case stack.parent is
							when SEC_UNITS_INTERNAL =>
								declare
									kw : string := f (line, 1);
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_name then
										expect_field_count (line, 2);
										unit_name := et_libraries.to_unit_name (f (line,2));

										-- Create a new symbol where unit_symbol is pointing at.
										-- The symbol assumes the appearance of the device.
										-- The symbol will be copied to the current unit later.
										case appearance is
											when et_libraries.SCH =>
												unit_symbol := new et_libraries.type_symbol' (
													appearance	=> et_libraries.SCH,
													others		=> <>);

											when et_libraries.SCH_PCB =>
												unit_symbol := new et_libraries.type_symbol' (
													appearance	=> et_libraries.SCH_PCB,
													others		=> <>);

											when others => 
												raise constraint_error; -- CS

										end case;
										
									elsif kw = keyword_position then -- position x 0.00 y 0.00
										expect_field_count (line, 5);

										-- extract unit position starting at field 2
										-- NOTE: this is the position of the unit inside the device editor !
										unit_position := to_position (line, 2);

									elsif kw = keyword_swap_level then
										expect_field_count (line, 2);
										unit_swap_level := to_swap_level (f (line, 2));

									elsif kw = keyword_add_level then
										expect_field_count (line, 2);
										unit_add_level := to_add_level (f (line, 2));
										
									else
										invalid_keyword (kw);
									end if;
								end;
								
							when SEC_UNITS_EXTERNAL =>
								declare
									kw : string := f (line, 1);
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_name then -- name A, B, ...
										expect_field_count (line, 2);
										unit_name := et_libraries.to_unit_name (f (line,2));

									elsif kw = keyword_position then -- position x 0.00 y 0.00
										expect_field_count (line, 5);

										-- extract unit position starting at field 2
										-- NOTE: this is the position of the unit inside the device editor !
										unit_external.position := to_position (line, 2);

									elsif kw = keyword_swap_level then -- swap_level 1
										expect_field_count (line, 2);
										unit_external.swap_level := to_swap_level (f (line, 2));

									elsif kw = keyword_add_level then -- add_level next
										expect_field_count (line, 2);
										unit_external.add_level := to_add_level (f (line, 2));

									elsif kw = keyword_file then -- file libraries/symbols/nand.sym
										expect_field_count (line, 2);
										unit_external.file := to_file_name (f (line, 2));
										
									else
										invalid_keyword (kw);
									end if;
								end;
								
							when others => invalid_section;
						end case;

					when SEC_SYMBOL =>
						case stack.parent is
							when SEC_UNIT =>
								case stack.parent (degree => 2) is
									when SEC_UNITS_INTERNAL => null;
									when others => invalid_section;
								end case;
								
							when others => invalid_section;
						end case;

					when SEC_DRAW =>
						case stack.parent is
							when SEC_SYMBOL => null;  -- nothing to do
							when others => invalid_section;
						end case;
						
					when SEC_LINE =>
						case stack.parent is
							when SEC_DRAW =>
								declare
									kw : string := f (line, 1);
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_start then -- start x 1 y 2
										expect_field_count (line, 5);

										-- extract the start position starting at field 2
										symbol_line.start_point := to_position (line,2);
										
									elsif kw = keyword_end then -- end x 0.00 y 0.00
										expect_field_count (line, 5);

										-- extract the end position starting at field 2
										symbol_line.end_point := to_position (line,2);

									elsif kw = keyword_width then
										expect_field_count (line, 2);
										symbol_line.width := to_distance (f (line, 2));
										
									else
										invalid_keyword (kw);
									end if;
								end;

							when others => invalid_section;
						end case;

					when SEC_ARC =>
						case stack.parent is
							when SEC_DRAW =>
								declare
									kw : string := f (line, 1);
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = shapes.keyword_center then -- center x 1 y 2
										expect_field_count (line, 5);

										-- extract the start position starting at field 2
										symbol_arc.center := to_position (line,2);

									elsif kw = keyword_start then -- start x 1 y 2
										expect_field_count (line, 5);

										-- extract the start position starting at field 2
										symbol_arc.start_point := to_position (line,2);
										
									elsif kw = keyword_end then -- end x 0.00 y 0.00
										expect_field_count (line, 5);

										-- extract the end position starting at field 2
										symbol_arc.end_point := to_position (line,2);

									elsif kw = keyword_width then
										expect_field_count (line, 2);
										symbol_arc.width := to_distance (f (line, 2));

									elsif kw = shapes.keyword_radius then
										expect_field_count (line, 2);
										symbol_arc.radius := to_distance (f (line, 2));
										
									else
										invalid_keyword (kw);
									end if;
								end;

							when others => invalid_section;
						end case;
						
					when SEC_CIRCLE =>
						case stack.parent is
							when SEC_DRAW =>
								declare
									kw : string := f (line, 1);
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = shapes.keyword_center then -- center x 1 y 2
										expect_field_count (line, 5);

										-- extract the start position starting at field 2
										symbol_circle.center := to_position (line,2);

									elsif kw = keyword_width then -- widht 0.2
										expect_field_count (line, 2);
										symbol_circle.width := to_distance (f (line, 2));

									elsif kw = shapes.keyword_radius then -- radius 5
										expect_field_count (line, 2);
										symbol_circle.radius := to_distance (f (line, 2));

									elsif kw = et_libraries.shapes.keyword_filled then -- filled yes/no
										expect_field_count (line, 2);
										symbol_circle.filled := et_libraries.to_circle_filled (f (line, 2));
										
									else
										invalid_keyword (kw);
									end if;
								end;

							when others => invalid_section;
						end case;
						
					when SEC_TEXTS =>
						case stack.parent is
							when SEC_SYMBOL => null; -- nothing to do
							when others => invalid_section;
						end case;

					when SEC_TEXT =>
						case stack.parent is
							when SEC_TEXTS =>
								declare
									kw : string := f (line, 1);
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_position then -- position x 1 y 2
										expect_field_count (line, 5);

										-- extract the text position starting at field 2
										symbol_text_position := to_position (line,2);

									elsif kw = keyword_content then -- content "dummy NAND gate"
										expect_field_count (line, 2);
										symbol_text_content := et_libraries.to_content (f (line, 2));

									elsif kw = keyword_size then -- size 5
										expect_field_count (line, 2);
										symbol_text_base.size := to_distance (f (line, 2));

									elsif kw = keyword_line_width then -- line_width 0.2
										expect_field_count (line, 2);
										symbol_text_base.line_width := to_distance (f (line, 2));

									elsif kw = keyword_rotation then -- rotation 90.0
										expect_field_count (line, 2);
										symbol_text_base.rotation := to_rotation (f (line, 2));
										
									elsif kw = keyword_style then -- style italic
										expect_field_count (line, 2);
										symbol_text_base.style := et_libraries.to_text_style (f (line, 2));

									elsif kw = keyword_alignment then -- alignment horizontal center vertical center
										expect_field_count (line, 5);
										symbol_text_base.alignment := to_alignment (line, 2);

									else
										invalid_keyword (kw);
									end if;
								end;

							when others => invalid_section;
						end case;
						
					when SEC_PLACEHOLDERS =>
						case stack.parent is
							when SEC_SYMBOL => null; -- nothing to do
							when others => invalid_section;
						end case;
						
					when SEC_PLACEHOLDER =>
						case stack.parent is
							when SEC_PLACEHOLDERS =>
								declare
									kw : string := f (line, 1);
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_position then -- position x 1 y 2
										expect_field_count (line, 5);

										-- extract the placeholder position starting at field 2
										symbol_text_position := to_position (line,2);

									elsif kw = keyword_meaning then -- meaning reference
										expect_field_count (line, 2);
										symbol_placeholder_meaning := et_libraries.to_text_meaning (f (line, 2));

									elsif kw = keyword_size then -- size 5
										expect_field_count (line, 2);
										symbol_text_base.size := to_distance (f (line, 2));

									elsif kw = keyword_line_width then -- line_width 0.2
										expect_field_count (line, 2);
										symbol_text_base.line_width := to_distance (f (line, 2));

									elsif kw = keyword_rotation then -- rotation 90.0
										expect_field_count (line, 2);
										symbol_text_base.rotation := to_rotation (f (line, 2));
										
									elsif kw = keyword_style then -- style italic
										expect_field_count (line, 2);
										symbol_text_base.style := et_libraries.to_text_style (f (line, 2));

									elsif kw = keyword_alignment then -- alignment horizontal center vertical center
										expect_field_count (line, 5);
										symbol_text_base.alignment := to_alignment (line, 2);

									else
										invalid_keyword (kw);
									end if;
								end;

							when others => invalid_section;
						end case;

					when SEC_PORTS =>
						case stack.parent is 
							when SEC_SYMBOL => null; -- nothing to do
							when others => invalid_section;
						end case;

					when SEC_PORT =>
						case stack.parent is
							when SEC_PORTS =>
								declare
									kw : string := f (line, 1);
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_position then -- position x 1 y 2
										expect_field_count (line, 5);

										-- extract the port position starting at field 2
										port.position := to_position (line,2);

									elsif kw = keyword_name then -- name I1A
										expect_field_count (line, 2);
										port_name := et_libraries.to_port_name (f (line, 2));

									elsif kw = keyword_length then -- length 5
										expect_field_count (line, 2);
										port.length := to_distance (f (line, 2));

									elsif kw = keyword_rotation then -- rotation 90.0
										expect_field_count (line, 2);
										port.rotation := to_rotation (f (line, 2));
										
									elsif kw = keyword_port_name_visible then -- port_name_visible yes/no
										expect_field_count (line, 2);
										port.port_name_visible := et_libraries.to_port_name_visible (f (line, 2));

									elsif kw = keyword_port_name_size then -- port_name_size 2.0
										expect_field_count (line, 2);
										port.port_name_size := to_distance (f (line, 2));

									elsif kw = keyword_terminal_name_visible then -- terminal_name_visible yes/no
										expect_field_count (line, 2);
										port.terminal_name_visible := et_libraries.to_terminal_name_visible (f (line, 2));

									elsif kw = keyword_terminal_name_size then -- terminal_name_size 2.0
										expect_field_count (line, 2);
										port.terminal_name_size := to_distance (f (line, 2));

									elsif kw = keyword_direction then -- direction BIDIR, PASSIVE, NOT_CONNECTED, ...
										expect_field_count (line, 2);
										port_direction := et_libraries.to_port_direction (f (line, 2));

									elsif kw = keyword_sensitivity_edge then -- sensitivity_edge rising/falling/any
										expect_field_count (line, 2);
										port_sensitivity_edge := et_libraries.to_sensitivity_edge (f (line, 2));

									elsif kw = keyword_sensitivity_level then -- sensitivity_level high/low
										expect_field_count (line, 2);
										port_sensitivity_level := et_libraries.to_sensitivity_level (f (line, 2));

									elsif kw = keyword_inverted then -- inverted yes/no
										expect_field_count (line, 2);
										port_output_inverted := et_libraries.to_output_inverted (f (line, 2));

									elsif kw = keyword_tristate then -- tristate yes/no
										expect_field_count (line, 2);
										port_output_tristate := et_libraries.to_output_tristate (f (line, 2));

									elsif kw = keyword_level then -- level positive/negative/zero
										expect_field_count (line, 2);
										port_power_level := et_libraries.to_power_level (f (line, 2));

									elsif kw = keyword_weakness then -- weakness none/pull0/weak1 ...
										expect_field_count (line, 2);
										port_output_weakness := et_libraries.to_output_weakness (f (line, 2));
										
									else
										invalid_keyword (kw);
									end if;
								end;

							when others => invalid_section;
						end case;
						
				end case;
			end if;

			exception when event: others =>
				log (text => "file " & to_string (file_name) & latin_1.space 
					 & affected_line (line) & to_string (line), console => true);
				raise;
			
		end process_line;

		previous_input : ada.text_io.file_type renames current_input;
		
	begin -- read_device_file
		log_indentation_up;
		log (text => "reading device model " & to_string (file_name) & " ...", level => log_threshold);
		log_indentation_up;
		
		-- test if container et_libraries.devices already contains a model
		-- named "file_name". If so, there would be no need to read the file_name again.
		if et_libraries.type_devices.contains (et_libraries.devices, file_name) then
			log (text => "already read -> skipped", level => log_threshold + 1);
		else
			-- If the model file is to be read, first check if the file exists.
			declare
				file : string := expand (to_string (file_name));
			begin
				if ada.directories.exists (file) then

					-- open device model file
					open (
						file => file_handle,
						mode => in_file, 
						name => file);

				else
					log (ERROR, "device model " & file &
						 " not found !", console => true);
					raise constraint_error;
				end if;
			end;

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
					ifs 			=> latin_1.space); -- fields are separated by space

				-- we are interested in lines that contain something. emtpy lines are skipped:
				if field_count (line) > 0 then
					process_line;
				end if;
			end loop;

			-- As a safety measure the top section must be reached finally.
			if stack.depth > 1 then 
				log (WARNING, write_section_stack_not_empty);
			end if;

			set_input (previous_input);
			close (file_handle);

			-- Assemble final device and insert it in et_libraries.devices:
			case appearance is
				when SCH_PCB => -- a real device

					-- If a value was specified (via an entry like "value 100R),
					-- check if it meets certain conventions regarding its prefix.
					-- The prefix gives information about the category of the device:
					if et_libraries.type_value.length (value) > 0 then
						if not conventions.value_valid (value, prefix) then
							log (WARNING, "default value of device model " &
								to_string (file_name) & 
								" not conformant with conventions !");
						end if;
					end if;

					et_libraries.type_devices.insert (
						container	=> et_libraries.devices, 
						key			=> file_name, -- libraries/devices/7400.dev
						new_item	=> (
								appearance		=> SCH_PCB,
								prefix			=> prefix, -- IC
								units_internal	=> units_internal,
								units_external	=> units_external,
								value			=> value,
								--partcode		=> partcode,
								variants		=> variants));

				when SCH => -- virtual device
					et_libraries.type_devices.insert (
						container	=> et_libraries.devices, 
						key			=> file_name, -- libraries/devices/power_gnd.dev
						new_item	=> (
								appearance		=> SCH,
								prefix			=> prefix, -- PWR
								units_internal	=> units_internal,
								units_external	=> units_external));

				when others => null; -- CS
			end case;
		end if;

		-- CS Check integrity of device: port terminal map, positions of units, ...
		-- (style guides, conventions ...)
		-- use function "last" to fetch latest device

		log_indentation_down;
		log_indentation_down;		

		exception when event: others =>
			if is_open (file_handle) then 
				set_input (previous_input);
				close (file_handle); 
			end if;
			raise;

	end read_device_file;
							  
	procedure save_symbol ( -- CS: testing requried
	-- Saves the given symbol model in a file specified by name.
		name			: in string; -- libraries/symbols/resistor.sym
		symbol			: in et_libraries.type_symbol; -- the actual symbol model
		log_threshold	: in et_string_processing.type_log_level) is
		use et_string_processing;
		file_handle : ada.text_io.file_type;
	begin
		log (text => name, level => log_threshold);

		create (
			file 	=> file_handle,
			mode	=> out_file,
			name	=> name);

		set_output (file_handle);
		
		-- write a nice header
		put_line (comment_mark & " " & et_general.system_name & " symbol");
		put_line (comment_mark & " " & date);
		put_line (comment_mark & " " & row_separator_double);
		new_line;

		write_symbol (symbol, log_threshold + 1);

		-- write footer
		new_line;		
		put_line (comment_mark & " " & row_separator_double);
		put_line (comment_mark & " symbol file end");
		new_line;
		
		reset_tab_depth;		
		
		set_output (standard_output);
		close (file_handle);

		exception when event: others =>
			log (text => ada.exceptions.exception_message (event));
			if is_open (file_handle) then
				close (file_handle);
			end if;
			raise;
		
	end save_symbol;

	
	procedure save_package (
	-- Saves the given package model in a file specified by name.
		name			: in string; -- libraries/packages/resistor.pac
		packge			: in et_packages.type_package; -- the actual package model
		log_threshold	: in et_string_processing.type_log_level) is
		use et_string_processing;
		use et_packages;
		use et_pcb_coordinates.geometry;
		
		file_handle : ada.text_io.file_type;
		
		use type_texts_with_content;
		
		procedure write_copper is
		-- This is about copper objects in either top or bottom.
		-- These objects have no connection to any pad or signal.

			use type_copper_lines;
			procedure write_line (cursor : in type_copper_lines.cursor) is begin
				line_begin;
				write_line (element (cursor));
				write_width (element (cursor).width);
				line_end;
			end write_line;

			use type_copper_arcs;
			procedure write_arc (cursor : in type_copper_arcs.cursor) is begin
				arc_begin;
				write_arc (element (cursor));
				write_width (element (cursor).width);
				arc_end;
			end write_arc;

			use et_packages.pac_copper_circles;
			procedure write_circle (cursor : in et_packages.pac_copper_circles.cursor) is begin
				write_circle_copper (element (cursor));
			end write_circle;

			use pac_copper_polygons_solid;
			procedure write_polygon (cursor : in pac_copper_polygons_solid.cursor) is begin
				fill_zone_begin;
				write_easing (element (cursor).easing);

				write_width_min (element (cursor).width_min);
				write_isolation (element (cursor).isolation);

				write_fill_stlye (element (cursor).fill_style);

				write_polygon_segments (shapes.type_polygon_base (element (cursor)));
				fill_zone_end;
			end write_polygon;

			use pac_copper_polygons_hatched;
			procedure write_polygon (cursor : in pac_copper_polygons_hatched.cursor) is begin
				fill_zone_begin;
				write_easing (element (cursor).easing);

				write_width_min (element (cursor).width_min);
				write_isolation (element (cursor).isolation);

				write_fill_stlye (element (cursor).fill_style);
				write_hatching (element (cursor).hatching);
				
				write_polygon_segments (shapes.type_polygon_base (element (cursor)));

				fill_zone_end;
			end write_polygon;

			use pac_copper_cutouts;
			procedure write_cutout (cursor : in pac_copper_cutouts.cursor) is begin
				cutout_zone_begin;
				write_easing (element (cursor).easing);				
				write_polygon_segments (shapes.type_polygon_base (element (cursor)));
				cutout_zone_end;
			end;
			
		begin -- write_copper
			section_mark (section_copper, HEADER);

			-- top
			section_mark (section_top, HEADER);			
			iterate (packge.copper.top.lines, write_line'access);
			iterate (packge.copper.top.arcs, write_arc'access);
			iterate (packge.copper.top.circles, write_circle'access);
			iterate (packge.copper.top.polygons.solid, write_polygon'access);
			iterate (packge.copper.top.polygons.hatched, write_polygon'access);
			iterate (packge.copper.top.cutouts, write_cutout'access);			
			iterate (packge.copper.top.texts, write_text'access);
			section_mark (section_top, FOOTER);

			-- bottom
			section_mark (section_bottom, HEADER);			
			iterate (packge.copper.bottom.lines, write_line'access);
			iterate (packge.copper.bottom.arcs, write_arc'access);
			iterate (packge.copper.bottom.circles, write_circle'access);
			iterate (packge.copper.bottom.polygons.solid, write_polygon'access);
			iterate (packge.copper.bottom.polygons.hatched, write_polygon'access);
			iterate (packge.copper.bottom.cutouts, write_cutout'access);
			iterate (packge.copper.bottom.texts, write_text'access);			
			section_mark (section_bottom, FOOTER);

			section_mark (section_copper, FOOTER);
		end write_copper;

		use pac_text_placeholders;		
		procedure write_placeholder (cursor : in pac_text_placeholders.cursor) is begin
			placeholder_begin;
			write (keyword => keyword_meaning, parameters => to_string (element (cursor).meaning));
			write_text_properties (element (cursor));
			placeholder_end;
		end write_placeholder;

		procedure write_silk_screen is 
			use type_silk_lines;
			use type_silk_arcs;
			use type_silk_circles;
			use pac_silk_polygons;
			use pac_silk_cutouts;
		begin
			section_mark (section_silk_screen, HEADER);

			-- top
			section_mark (section_top, HEADER);
			iterate (packge.silk_screen.top.lines, write_line'access);
			iterate (packge.silk_screen.top.arcs, write_arc'access);
			iterate (packge.silk_screen.top.circles, write_circle'access);
			iterate (packge.silk_screen.top.polygons, write_polygon'access);
			iterate (packge.silk_screen.top.cutouts, write_cutout'access);
			iterate (packge.silk_screen.top.texts, write_text'access);
			iterate (packge.silk_screen.top.placeholders, write_placeholder'access);
			section_mark (section_top, FOOTER);
			
			-- bottom
			section_mark (section_bottom, HEADER);
			iterate (packge.silk_screen.bottom.lines, write_line'access);
			iterate (packge.silk_screen.bottom.arcs, write_arc'access);
			iterate (packge.silk_screen.bottom.circles, write_circle'access);
			iterate (packge.silk_screen.bottom.polygons, write_polygon'access);
			iterate (packge.silk_screen.bottom.cutouts, write_cutout'access);
			iterate (packge.silk_screen.bottom.texts, write_text'access);
			iterate (packge.silk_screen.bottom.placeholders, write_placeholder'access);
			section_mark (section_bottom, FOOTER);

			section_mark (section_silk_screen, FOOTER);			
		end write_silk_screen;

		procedure write_assembly_documentation is 
			use type_doc_lines;
			use type_doc_arcs;
			use type_doc_circles;
			use pac_doc_polygons;
			use pac_doc_cutouts;
		begin
			section_mark (section_assembly_doc, HEADER);

			-- top
			section_mark (section_top, HEADER);
			iterate (packge.assembly_documentation.top.lines, write_line'access);
			iterate (packge.assembly_documentation.top.arcs, write_arc'access);
			iterate (packge.assembly_documentation.top.circles, write_circle'access);
			iterate (packge.assembly_documentation.top.polygons, write_polygon'access);
			iterate (packge.assembly_documentation.top.cutouts, write_cutout'access);
			iterate (packge.assembly_documentation.top.texts, write_text'access);
			iterate (packge.assembly_documentation.top.placeholders, write_placeholder'access);
			section_mark (section_top, FOOTER);
			
			-- bottom
			section_mark (section_bottom, HEADER);
			iterate (packge.assembly_documentation.bottom.lines, write_line'access);
			iterate (packge.assembly_documentation.bottom.arcs, write_arc'access);
			iterate (packge.assembly_documentation.bottom.circles, write_circle'access);
			iterate (packge.assembly_documentation.bottom.polygons, write_polygon'access);
			iterate (packge.assembly_documentation.bottom.cutouts, write_cutout'access);			
			iterate (packge.assembly_documentation.bottom.texts, write_text'access);
			iterate (packge.assembly_documentation.bottom.placeholders, write_placeholder'access);
			section_mark (section_bottom, FOOTER);

			section_mark (section_assembly_doc, FOOTER);
		end write_assembly_documentation;
		
		procedure write_keepout is 
			use type_keepout_lines;
			use type_keepout_arcs;
			use type_keepout_circles;
			use type_keepout_polygons;
			use pac_keepout_cutouts;
		begin
			section_mark (section_keepout, HEADER);

			-- top
			section_mark (section_top, HEADER);
			iterate (packge.keepout.top.lines, write_line'access);
			iterate (packge.keepout.top.arcs, write_arc'access);
			iterate (packge.keepout.top.circles, write_circle'access);
			iterate (packge.keepout.top.polygons, write_polygon'access);
			iterate (packge.keepout.top.cutouts, write_cutout'access);
			section_mark (section_top, FOOTER);
			
			-- bottom
			section_mark (section_bottom, HEADER);
			iterate (packge.keepout.bottom.lines, write_line'access);
			iterate (packge.keepout.bottom.arcs, write_arc'access);
			iterate (packge.keepout.bottom.circles, write_circle'access);
			iterate (packge.keepout.bottom.polygons, write_polygon'access);			
			iterate (packge.keepout.bottom.cutouts, write_cutout'access);
			section_mark (section_bottom, FOOTER);

			section_mark (section_keepout, FOOTER);			
		end write_keepout;

		procedure write_stop_mask is 
			use type_stop_lines;
			use type_stop_arcs;
			use type_stop_circles;
			use type_stop_polygons;
			use pac_stop_cutouts;
		begin
			section_mark (section_stop_mask, HEADER);

			-- top
			section_mark (section_top, HEADER);
			iterate (packge.stop_mask.top.lines, write_line'access);
			iterate (packge.stop_mask.top.arcs, write_arc'access);
			iterate (packge.stop_mask.top.circles, write_circle'access);
			iterate (packge.stop_mask.top.polygons, write_polygon'access);
			iterate (packge.stop_mask.top.cutouts, write_cutout'access);
			section_mark (section_top, FOOTER);
			
			-- bottom
			section_mark (section_bottom, HEADER);
			iterate (packge.stop_mask.bottom.lines, write_line'access);
			iterate (packge.stop_mask.bottom.arcs, write_arc'access);
			iterate (packge.stop_mask.bottom.circles, write_circle'access);
			iterate (packge.stop_mask.bottom.polygons, write_polygon'access);			
			iterate (packge.stop_mask.bottom.cutouts, write_cutout'access);
			section_mark (section_bottom, FOOTER);

			section_mark (section_stop_mask, FOOTER);			
		end write_stop_mask;

		procedure write_stencil is 
			use type_stencil_lines;
			use type_stencil_arcs;
			use type_stencil_circles;
			use type_stencil_polygons;
			use pac_stencil_cutouts;
		begin
			section_mark (section_stencil, HEADER);

			-- top
			section_mark (section_top, HEADER);
			iterate (packge.stencil.top.lines, write_line'access);
			iterate (packge.stencil.top.arcs, write_arc'access);
			iterate (packge.stencil.top.circles, write_circle'access);
			iterate (packge.stencil.top.polygons, write_polygon'access);
			iterate (packge.stencil.top.cutouts, write_cutout'access);
			section_mark (section_top, FOOTER);
			
			-- bottom
			section_mark (section_bottom, HEADER);
			iterate (packge.stencil.bottom.lines, write_line'access);
			iterate (packge.stencil.bottom.arcs, write_arc'access);
			iterate (packge.stencil.bottom.circles, write_circle'access);
			iterate (packge.stencil.bottom.polygons, write_polygon'access);
			iterate (packge.stencil.bottom.cutouts, write_cutout'access);			
			section_mark (section_bottom, FOOTER);

			section_mark (section_stencil, FOOTER);			
		end write_stencil;

		procedure write_route_restrict is 
			use type_route_restrict_lines;
			use type_route_restrict_arcs;
			use type_route_restrict_circles;
			use type_route_restrict_polygons;
			use pac_route_restrict_cutouts;
		begin
			section_mark (section_route_restrict, HEADER);

			iterate (packge.route_restrict.lines, write_line'access);
			iterate (packge.route_restrict.arcs, write_arc'access);
			iterate (packge.route_restrict.circles, write_circle'access);
			iterate (packge.route_restrict.polygons, write_polygon'access);
			iterate (packge.route_restrict.cutouts, write_cutout'access);

			section_mark (section_route_restrict, FOOTER);			
		end write_route_restrict;

		procedure write_via_restrict is 
			use type_via_restrict_lines;
			use type_via_restrict_arcs;
			use type_via_restrict_circles;
			use type_via_restrict_polygons;
			use pac_via_restrict_cutouts;
		begin
			section_mark (section_via_restrict, HEADER);

			iterate (packge.via_restrict.lines, write_line'access);
			iterate (packge.via_restrict.arcs, write_arc'access);
			iterate (packge.via_restrict.circles, write_circle'access);
			iterate (packge.via_restrict.polygons, write_polygon'access);			
			iterate (packge.via_restrict.cutouts, write_cutout'access);
			
			section_mark (section_via_restrict, FOOTER);			
		end write_via_restrict;

		procedure write_contour is -- about PCB contours
			use type_pcb_contour_lines;
			use type_pcb_contour_arcs;
			use type_pcb_contour_circles;
		begin
			section_mark (section_pcb_contours, HEADER);

			iterate (packge.pcb_contour.lines, write_line'access);
			iterate (packge.pcb_contour.arcs, write_arc'access);
			iterate (packge.pcb_contour.circles, write_circle'access);

			section_mark (section_pcb_contours, FOOTER);
		end write_contour;
		
		-- CS currently no need for plated millings not terminal related
-- 		procedure write_contour_plated is begin
-- 			section_mark (section_pcb_contour_plated, HEADER);
-- 
-- 			iterate (packge.pcb_contour_plated.lines, write_line'access);
-- 			iterate (packge.pcb_contour_plated.arcs, write_arc'access);
-- 			iterate (packge.pcb_contour_plated.circles, write_circle'access);
-- 
-- 			section_mark (section_pcb_contour_plated, FOOTER);
-- 		end write_contour_plated;

		procedure write_package_contour is begin
			section_mark (section_pac_3d_contours, HEADER);
			-- CS
			section_mark (section_pac_3d_contours, FOOTER);
		end write_package_contour;

		procedure write_terminals is 
			use type_terminals;
			terminal_cursor : type_terminals.cursor := packge.terminals.first;

			procedure write_plated_millings (millings : in type_plated_millings) is begin
				section_mark (section_pad_millings, HEADER);
				write_polygon_segments (shapes.type_polygon_base (millings));
				section_mark (section_pad_millings, FOOTER);
			end write_plated_millings;
			
		begin -- write_terminals
			section_mark (section_terminals, HEADER);
			
			while terminal_cursor /= type_terminals.no_element loop
				section_mark (section_terminal, HEADER);
				write (keyword => keyword_name, parameters => space & et_libraries.to_string (key (terminal_cursor)));
				write (keyword => keyword_assembly_technology, parameters => to_string (element (terminal_cursor).technology));
				write (keyword => keyword_position, parameters => position (element (terminal_cursor).position));
				
				case element (terminal_cursor).technology is
					when THT =>
						-- pad contour top
						section_mark (section_pad_contours_tht, HEADER);
						
						section_mark (section_top, HEADER);
						write_polygon_segments (shapes.type_polygon_base (element (terminal_cursor).pad_shape_tht.top));
						section_mark (section_top, FOOTER);

						-- pad contour bottom
						section_mark (section_bottom, HEADER);
						write_polygon_segments (shapes.type_polygon_base (element (terminal_cursor).pad_shape_tht.bottom));
						section_mark (section_bottom, FOOTER);
						
						section_mark (section_pad_contours_tht, FOOTER);

						-- copper width in inner layers
						write (keyword => keyword_width_inner_layers, 
							   parameters => to_string (element (terminal_cursor).width_inner_layers));
						
						-- A THT terminal can have a drilled or a milled hole:
						write (keyword => keyword_tht_hole, parameters => to_string (element (terminal_cursor).tht_hole));

						case element (terminal_cursor).tht_hole is
							when DRILLED => 
								write (keyword_drill_size, parameters => to_string (element (terminal_cursor).drill_size));
								
							when MILLED => 
								write_plated_millings (element (terminal_cursor).millings);
						end case;
						
					when SMT =>
						-- pad contour
						section_mark (section_pad_contours_smt, HEADER);
						write_polygon_segments (shapes.type_polygon_base (element (terminal_cursor).pad_shape));
						section_mark (section_pad_contours_smt, FOOTER);
						
						write (keyword => et_pcb_coordinates.keyword_face, parameters => et_pcb_coordinates.to_string (element (terminal_cursor).face));
						write (keyword => keyword_stop_mask, parameters => to_string (element (terminal_cursor).stop_mask));
						write (keyword => keyword_solder_paste, parameters => to_string (element (terminal_cursor).solder_paste));	
				end case;


				section_mark (section_terminal, FOOTER);
				next (terminal_cursor);
			end loop;
			
			section_mark (section_terminals, FOOTER);
		end write_terminals;
		
	begin -- save_package
		log (text => name, level => log_threshold);

		create (
			file 	=> file_handle,
			mode	=> out_file,
			name	=> name);

		set_output (file_handle);
		
		-- write a nice header
		put_line (comment_mark & " " & et_general.system_name & " package");
		put_line (comment_mark & " " & date);
		put_line (comment_mark & " " & row_separator_double);
		new_line;

		reset_tab_depth;

		write (keyword => keyword_description, space => true, wrap => true, 
			   parameters => to_string (packge.description));

		write (keyword => keyword_appearance, parameters => to_string (packge.appearance));
		write (keyword => keyword_assembly_technology, parameters => to_string (packge.technology));

		write_silk_screen;
		write_assembly_documentation;
		write_keepout;
		write_copper;
		write_stop_mask;
		write_stencil;
		write_route_restrict;
		write_via_restrict;
		write_contour; -- pcb contour plated
		-- write_contour_plated; -- pcb contour -- CS currently no need
		write_terminals; -- incl. pad properties, drill sizes, millings, ...

		-- 3D stuff
		if packge.appearance = REAL then
			null;
			--write_package_contour;  -- CS uncomment when 3d support available
		end if;

		-- write footer
		new_line;
		put_line (comment_mark & " " & row_separator_double);
		put_line (comment_mark & " package model file end");
		new_line;
		
		reset_tab_depth;
		
		set_output (standard_output);
		close (file_handle);

		exception when event: others =>
			log (text => ada.exceptions.exception_message (event));
			if is_open (file_handle) then
				close (file_handle);
			end if;
			raise;

	end save_package;

	function to_string (section : in type_section_name_module) return string is
	-- Converts a section like SEC_NET to a string "net".
		len : positive := type_section_name_module'image (section)'length;
	begin
		return to_lower (type_section_name_module'image (section) (5..len));
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
		package stack is new stack_lifo (
			item	=> type_section_name_module,
			max 	=> max_section_depth);

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
				elsif f (line, place) = keyword_pos_x then
					--set_x (point, to_distance (f (line, place + 1)));
					set (X, to_distance (f (line, place + 1)), point);

				-- We expect after the y the corresponding value for y
				elsif f (line, place) = keyword_pos_y then
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
				if f (line, place) = keyword_pos_x then
					size.x := to_distance (f (line, place + 1));

				-- We expect after the y the corresponding value for y
				elsif f (line, place) = keyword_pos_y then
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
				if f (line, place) = keyword_pos_x then
					set (point => point, axis => X, value => to_distance (f (line, place + 1)));

				-- We expect after the y the corresponding value for y
				elsif f (line, place) = keyword_pos_y then
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

		
		frame_template_schematic	: et_libraries.type_frame_template_name.bounded_string;	-- $ET_FRAMES/drawing_frame_version_1.frm
		-- CS frame_count_schematic		: et_coordinates.type_submodule_sheet_number := et_coordinates.type_submodule_sheet_number'first; -- 10 frames
		frame_template_board		: et_libraries.type_frame_template_name.bounded_string;	-- $ET_FRAMES/drawing_frame_version_2.frm

		-- submodules
		submodule_port			: submodules.type_submodule_port;
		submodule_port_name		: et_general.type_net_name.bounded_string; -- RESET
		submodule_ports			: submodules.type_submodule_ports.map;
		submodule_name 			: et_general.type_module_instance_name.bounded_string; -- MOT_DRV_3
		submodule				: submodules.type_submodule;

		note : et_schematic.type_text;

		-- The temporarily device will exist where "device" points at:
		device					: access et_schematic.type_device;
		
		device_name				: et_libraries.type_device_name; -- C12
		device_model			: et_libraries.type_device_model_file.bounded_string; -- ../libraries/transistor/pnp.dev
		device_value			: et_libraries.type_value.bounded_string; -- 470R
		device_appearance		: et_schematic.type_appearance_schematic;
		--device_unit				: et_schematic.type_unit;
		--device_unit_rotation	: et_coordinates.type_rotation := geometry.zero_rotation;
		device_unit_mirror		: et_schematic.type_mirror := et_schematic.NO;
		device_unit_name		: et_libraries.type_unit_name.bounded_string; -- GPIO_BANK_1
		device_unit_position	: et_coordinates.type_position; -- x,y,sheet,rotation

		-- assembly variants
		assembly_variant_name			: et_general.type_variant_name.bounded_string; -- low_cost
		assembly_variant_description	: assembly_variants.type_description; -- "variant without temp. sensor"
		assembly_variant_devices		: assembly_variants.type_devices.map;
		assembly_variant_submodules		: assembly_variants.type_submodules.map;
		
		-- temporarily collection of units:
		device_units			: et_schematic.type_units.map; -- PWR, A, B, ...
		
		device_partcode			: material.type_partcode.bounded_string;
		device_purpose			: et_libraries.type_device_purpose.bounded_string;
		device_variant			: et_libraries.type_component_variant_name.bounded_string; -- D, N
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
		unit_placeholder			: et_libraries.type_text_basic;
		unit_placeholder_position	: et_coordinates.geometry.type_point;
		unit_placeholder_meaning	: et_libraries.type_text_meaning := et_libraries.type_text_meaning'first;
		unit_placeholder_reference	: et_libraries.type_text_placeholder (meaning => et_libraries.NAME);
		unit_placeholder_value		: et_libraries.type_text_placeholder (meaning => et_libraries.VALUE);
		unit_placeholder_purpose	: et_libraries.type_text_placeholder (meaning => et_libraries.PURPOSE);

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
			kw : string := f (line, 1);
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
				begin
					log (text => "drawing frame schematic " & et_libraries.to_string (frame_template_schematic), level => log_threshold + 1);
					module.frame_template_schematic := frame_template_schematic;
				end set_frame_schematic;
				
				procedure set_frame_board (
					module_name	: in type_module_name.bounded_string;
					module		: in out et_schematic.type_module) is
				begin
					log (text => "drawing frame board " & et_libraries.to_string (frame_template_board), level => log_threshold + 1);
					module.frame_template_board := frame_template_board;
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
					use et_libraries;
				begin
					log_indentation_up;
					-- log (text => "unit " & to_string (device_unit_name), log_threshold + 1);
					-- No good idea. Confuses operator because units are collected BEFORE the device is complete.
					
					-- Depending on the appearance of the device, a virtual or real unit
					-- is inserted in the unit list of the device.
					
					case device_appearance is
						when SCH =>
							et_schematic.type_units.insert (
								container	=> device_units,
								key			=> device_unit_name,
-- 								new_item	=> (device_unit with 
-- 												position	=> device_unit_position,
-- 												appearance	=> et_libraries.SCH));
								new_item	=> (
-- 									rotation	=> device_unit_rotation,
-- 									mirror		=> device_unit_mirror,
-- 									position	=> device_unit_position,
-- 									appearance	=> et_libraries.SCH));
									appearance	=> et_libraries.SCH,
									mirror		=> device_unit_mirror,
									position	=> device_unit_position));
												   
						when SCH_PCB =>
							-- A unit of a real device has placeholders:
							et_schematic.type_units.insert (
								container	=> device_units,
								key			=> device_unit_name,
-- 								new_item	=> (device_unit with
								new_item	=> (
-- 									rotation	=> device_unit_rotation,
									mirror		=> device_unit_mirror,

									position	=> device_unit_position,
									appearance	=> et_libraries.SCH_PCB,

									-- The placeholders for reference, value and purpose have
									-- been built and can now be assigned to the unit:
									name		=> unit_placeholder_reference,
									value 		=> unit_placeholder_value,
									purpose		=> unit_placeholder_purpose));
					end case;

					-- clean up for next unit
					device_unit_position := zero_position;
					device_unit_name := unit_name_default;
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
					use et_libraries;
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
					unit_placeholder_meaning := et_libraries.type_text_meaning'first;
					unit_placeholder_position := geometry.origin;
					
				end build_unit_placeholder;

				procedure insert_device (
					module_name	: in type_module_name.bounded_string;
					module		: in out et_schematic.type_module) is
					use et_schematic;
					use et_libraries;
					device_cursor : et_schematic.type_devices.cursor;
					inserted : boolean;

					function get_package_name return type_component_package_name.bounded_string is
					-- Derives package name from device.model and device.variant.
					-- Checks if variant exits in device.model.
						name : type_component_package_name.bounded_string; -- S_SO14 -- to be returned
						device_cursor : et_libraries.type_devices.cursor;

						procedure query_variants (
							model	: in type_device_model_file.bounded_string; -- libraries/devices/7400.dev
							dev_lib	: in et_libraries.type_device) -- a device in the library 
							is
							use type_component_variants;
							variant_cursor : type_component_variants.cursor;
							use ada.directories;
						begin -- query_variants
							-- Locate the variant (specified by the device in the module) in
							-- the device model.
							variant_cursor := type_component_variants.find (
								container	=> dev_lib.variants,
								key			=> device.variant); -- the variant name from the module !

							-- The variant should be there. Otherwise abort.
							if variant_cursor = type_component_variants.no_element then
								log (ERROR, "variant " & to_string (device.variant) &
									" not available in device model " & to_string (model) & " !", console => true);
								raise constraint_error;
							else
								name := to_package_name (base_name (to_string (element (variant_cursor).package_model)));
							end if;
						end;
						
					begin -- get_package_name
						log_indentation_up;
						log (text => "verifying package variant " & to_string (device.variant) &
								" in device model " & to_string (device.model) & " ... ", level => log_threshold + 2);

						-- Locate the device in the library. CS: It should be there, otherwise exception arises here:
						device_cursor := et_libraries.type_devices.find (
							container	=> et_libraries.devices,
							key			=> device.model); -- libraries/devices/7400.dev

						-- Query package variants
						et_libraries.type_devices.query_element (
							position	=> device_cursor,
							process		=> query_variants'access);
						
						log_indentation_down;
						return name;
					end get_package_name;
					
				begin -- insert_device
					log (text => "device " & et_libraries.to_string (device_name), level => log_threshold + 1);
					log_indentation_up;

					if not conventions.prefix_valid (device_name) then 
						--log (message_warning & "prefix of device " & et_libraries.to_string (device_name) 
						--	 & " not conformant with conventions !");
						null; -- CS output something helpful
					end if;
					
					-- assign temporarily variable for model:
					device.model := device_model;

					-- assign appearance specific temporarily variables and write log information
					if device.appearance = et_libraries.SCH_PCB then

						if not et_libraries.value_characters_valid (device_value) then
							log (WARNING, "value of " & et_libraries.to_string (device_name) &
								 " contains invalid characters !");
							log_indentation_reset;
							value_invalid (to_string (device_value));
						end if;
						
						log (text => "value " & et_libraries.to_string (device_value), level => log_threshold + 2);
						device.value	:= device_value;
						if not conventions.value_valid (device_value, prefix (device_name)) then
							log (WARNING, "value of " & et_libraries.to_string (device_name) &
								" not conformant with conventions !");
						end if;

						log (text => "partcode " & material.to_string (device_partcode), level => log_threshold + 2);
						if material.partcode_characters_valid (device_partcode) then
							device.partcode	:= device_partcode;
						else
							log_indentation_reset;
							material.partcode_invalid (material.to_string (device_partcode));
						end if;

						log (text => "purpose " & et_libraries.to_string (device_purpose), level => log_threshold + 2);
						if et_libraries.purpose_characters_valid (device_purpose) then
							device.purpose	:= device_purpose;
						else
							log_indentation_reset;
							purpose_invalid (to_string (device_purpose));
						end if;

						log (text => "variant " & et_libraries.to_string (device_variant), level => log_threshold + 2);
						et_libraries.check_variant_name_characters (device_variant);
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
						log (ERROR, "device name " & et_libraries.to_string (device_name) & " already used !",
								console => true);
						raise constraint_error;
					end if;

					-- read the device model (like ../libraries/transistor/pnp.dev)
					read_device_file (device.model, log_threshold + 2);

					if device.appearance = et_libraries.SCH_PCB then
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
					device_purpose	:= type_device_purpose.to_bounded_string ("");
					device_partcode := material.type_partcode.to_bounded_string ("");
					device_variant	:= to_component_variant_name ("");

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
											new_item	=> (shapes.type_line (board_line) with board_line_width));

									when ASSEMBLY_DOCUMENTATION =>
										type_doc_lines.append (
											container	=> module.board.assy_doc.top.lines,
											new_item	=> (shapes.type_line (board_line) with board_line_width));

									when STENCIL =>
										type_stencil_lines.append (
											container	=> module.board.stencil.top.lines,
											new_item	=> (shapes.type_line (board_line) with board_line_width));
										
									when STOP_MASK =>
										type_stop_lines.append (
											container	=> module.board.stop_mask.top.lines,
											new_item	=> (shapes.type_line (board_line) with board_line_width));

									when KEEPOUT =>
										type_keepout_lines.append (
											container	=> module.board.keepout.top.lines,
											new_item	=> (shapes.type_line (board_line) with null record));
										
								end case;
								
							when BOTTOM => null;
								case layer is
									when SILK_SCREEN =>
										type_silk_lines.append (
											container	=> module.board.silk_screen.bottom.lines,
											new_item	=> (shapes.type_line (board_line) with board_line_width));

									when ASSEMBLY_DOCUMENTATION =>
										type_doc_lines.append (
											container	=> module.board.assy_doc.bottom.lines,
											new_item	=> (shapes.type_line (board_line) with board_line_width));
										
									when STENCIL =>
										type_stencil_lines.append (
											container	=> module.board.stencil.bottom.lines,
											new_item	=> (shapes.type_line (board_line) with board_line_width));
										
									when STOP_MASK =>
										type_stop_lines.append (
											container	=> module.board.stop_mask.bottom.lines,
											new_item	=> (shapes.type_line (board_line) with board_line_width));

									when KEEPOUT =>
										type_keepout_lines.append (
											container	=> module.board.keepout.bottom.lines,
											new_item	=> (shapes.type_line (board_line) with null record));

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
											new_item	=> (shapes.type_arc (board_arc) with board_line_width));

									when ASSEMBLY_DOCUMENTATION =>
										type_doc_arcs.append (
											container	=> module.board.assy_doc.top.arcs,
											new_item	=> (shapes.type_arc (board_arc) with board_line_width));

									when STENCIL =>
										type_stencil_arcs.append (
											container	=> module.board.stencil.top.arcs,
											new_item	=> (shapes.type_arc (board_arc) with board_line_width));
										
									when STOP_MASK =>
										type_stop_arcs.append (
											container	=> module.board.stop_mask.top.arcs,
											new_item	=> (shapes.type_arc (board_arc) with board_line_width));

									when KEEPOUT =>
										type_keepout_arcs.append (
											container	=> module.board.keepout.top.arcs,
											new_item	=> (shapes.type_arc (board_arc) with null record));
								end case;
								
							when BOTTOM => null;
								case layer is
									when SILK_SCREEN =>
										type_silk_arcs.append (
											container	=> module.board.silk_screen.bottom.arcs,
											new_item	=> (shapes.type_arc (board_arc) with board_line_width));

									when ASSEMBLY_DOCUMENTATION =>
										type_doc_arcs.append (
											container	=> module.board.assy_doc.bottom.arcs,
											new_item	=> (shapes.type_arc (board_arc) with board_line_width));
										
									when STENCIL =>
										type_stencil_arcs.append (
											container	=> module.board.stencil.bottom.arcs,
											new_item	=> (shapes.type_arc (board_arc) with board_line_width));
										
									when STOP_MASK =>
										type_stop_arcs.append (
											container	=> module.board.stop_mask.bottom.arcs,
											new_item	=> (shapes.type_arc (board_arc) with board_line_width));

									when KEEPOUT =>
										type_keepout_arcs.append (
											container	=> module.board.keepout.bottom.arcs,
											new_item	=> (shapes.type_arc (board_arc) with null record));
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
						use et_packages.shapes;
						
						procedure append_silk_polygon_top is begin
							case board_fill_style is 
								when SOLID =>
									pac_silk_polygons.append (
										container	=> module.board.silk_screen.top.polygons,
										new_item	=> (shapes.type_polygon_base (polygon) with 
														fill_style 	=> SOLID,
														easing		=> board_easing));

								when HATCHED =>
									pac_silk_polygons.append (
										container	=> module.board.silk_screen.top.polygons,
										new_item	=> (shapes.type_polygon_base (polygon) with 
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
										new_item	=> (shapes.type_polygon_base (polygon) with 
														fill_style 	=> SOLID,
														easing		=> board_easing));

								when HATCHED =>
									pac_silk_polygons.append (
										container	=> module.board.silk_screen.bottom.polygons,
										new_item	=> (shapes.type_polygon_base (polygon) with 
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
										new_item	=> (shapes.type_polygon_base (polygon) with 
														easing		=> board_easing,
														fill_style 	=> SOLID));

								when HATCHED =>
									pac_doc_polygons.append (
										container	=> module.board.assy_doc.top.polygons,
										new_item	=> (shapes.type_polygon_base (polygon) with 
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
										new_item	=> (shapes.type_polygon_base (polygon) with 
														easing		=> board_easing,
														fill_style 	=> SOLID));

								when HATCHED =>
									pac_doc_polygons.append (
										container	=> module.board.assy_doc.bottom.polygons,
										new_item	=> (shapes.type_polygon_base (polygon) with 
														fill_style	=> HATCHED,
														easing		=> board_easing,
														hatching	=> board_hatching));
							end case;
						end;

						procedure append_keepout_polygon_top is begin
							type_keepout_polygons.append (
								container	=> module.board.keepout.top.polygons, 
								new_item	=> (shapes.type_polygon_base (polygon) with
												filled	=> board_filled));
						end;

						procedure append_keepout_polygon_bottom is begin
							type_keepout_polygons.append (
								container	=> module.board.keepout.bottom.polygons, 
								new_item	=> (shapes.type_polygon_base (polygon) with
												filled	=> board_filled));
						end;

						procedure append_stencil_polygon_top is begin
							case board_fill_style is
								when SOLID =>
									type_stencil_polygons.append (
										container	=> module.board.stencil.top.polygons,
										new_item	=> (shapes.type_polygon_base (polygon) with
														fill_style	=> SOLID,
														easing		=> board_easing));

								when HATCHED =>
									type_stencil_polygons.append (
										container	=> module.board.stencil.top.polygons,
										new_item	=> (shapes.type_polygon_base (polygon) with
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
										new_item	=> (shapes.type_polygon_base (polygon) with
														fill_style	=> SOLID,
														easing		=> board_easing));

								when HATCHED =>
									type_stencil_polygons.append (
										container	=> module.board.stencil.bottom.polygons,
										new_item	=> (shapes.type_polygon_base (polygon) with
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
										new_item	=> (shapes.type_polygon_base (polygon) with
														fill_style	=> SOLID,
														easing		=> board_easing));

								when HATCHED =>
									type_stop_polygons.append (
										container	=> module.board.stop_mask.top.polygons,
										new_item	=> (shapes.type_polygon_base (polygon) with
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
										new_item	=> (shapes.type_polygon_base (polygon) with
														fill_style	=> SOLID,
														easing		=> board_easing));

								when HATCHED =>
									type_stop_polygons.append (
										container	=> module.board.stop_mask.bottom.polygons,
										new_item	=> (shapes.type_polygon_base (polygon) with
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
						use et_packages.shapes;
						
						procedure append_silk_cutout_top is begin
							pac_silk_cutouts.append (
								container	=> module.board.silk_screen.top.cutouts,
								new_item	=> (shapes.type_polygon_base (polygon) with 
											easing		=> board_easing));
						end;

						procedure append_silk_cutout_bottom is begin
							pac_silk_cutouts.append (
								container	=> module.board.silk_screen.bottom.cutouts,
								new_item	=> (shapes.type_polygon_base (polygon) with 
											easing		=> board_easing));
						end;
						
						procedure append_assy_doc_cutout_top is begin
							pac_doc_cutouts.append (
								container	=> module.board.assy_doc.top.cutouts,
								new_item	=> (shapes.type_polygon_base (polygon) with 
												easing		=> board_easing));
						end;

						procedure append_assy_doc_cutout_bottom is begin
							pac_doc_cutouts.append (
								container	=> module.board.assy_doc.bottom.cutouts,
								new_item	=> (shapes.type_polygon_base (polygon) with 
												easing		=> board_easing));
						end;

						procedure append_keepout_cutout_top is begin
							pac_keepout_cutouts.append (
								container	=> module.board.keepout.top.cutouts, 
								new_item	=> (shapes.type_polygon_base (polygon) with
												easing		=> board_easing));
						end;

						procedure append_keepout_cutout_bottom is begin
							pac_keepout_cutouts.append (
								container	=> module.board.keepout.bottom.cutouts, 
								new_item	=> (shapes.type_polygon_base (polygon) with
												easing		=> board_easing));
						end;

						procedure append_stencil_cutout_top is begin
							pac_stencil_cutouts.append (
								container	=> module.board.stencil.top.cutouts,
								new_item	=> (shapes.type_polygon_base (polygon) with
												easing		=> board_easing));
						end;

						procedure append_stencil_cutout_bottom is begin
							pac_stencil_cutouts.append (
								container	=> module.board.stencil.bottom.cutouts,
								new_item	=> (shapes.type_polygon_base (polygon) with
												easing		=> board_easing));
						end;

						procedure append_stop_cutout_top is begin
							pac_stop_cutouts.append (
								container	=> module.board.stop_mask.top.cutouts,
								new_item	=> (shapes.type_polygon_base (polygon) with
												easing		=> board_easing));
						end;

						procedure append_stop_cutout_bottom is begin
							pac_stop_cutouts.append (
								container	=> module.board.stop_mask.bottom.cutouts,
								new_item	=> (shapes.type_polygon_base (polygon) with
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
							new_item	=> (shapes.type_polygon_base (polygon) with 
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
							new_item	=> (shapes.type_polygon_base (polygon) with 
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
					use et_packages.shapes;
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
							new_item	=> (shapes.type_line (board_line) with 
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
							new_item	=> (shapes.type_arc (board_arc) with 
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
							new_item	=> (shapes.type_polygon_base (polygon) with 
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
							new_item	=> (shapes.type_line (board_line) with 
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
							new_item	=> (shapes.type_arc (board_arc) with 
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
							new_item	=> (shapes.type_polygon_base (polygon) with 
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
					use et_packages.shapes;
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
							new_item	=> (et_packages.shapes.type_line (board_line) with
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
							new_item	=> (et_packages.shapes.type_arc (board_arc) with
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
							new_item	=> (et_packages.shapes.type_line (board_line) with board_lock_status));
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
							new_item	=> (et_packages.shapes.type_arc (board_arc) with board_lock_status));
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
							new_item	=> (et_packages.shapes.type_circle (board_circle) with board_lock_status));
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
					cursor : type_variants.cursor;
				begin
					log (text => "assembly variant " & 
						 enclose_in_quotes (to_variant (assembly_variant_name)), level => log_threshold + 2);

					-- insert variant in container variants
					type_variants.insert (
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
					assembly_variant_devices := type_devices.empty_map;
					assembly_variant_submodules := type_submodules.empty_map;
					
				end insert_assembly_variant;

				procedure build_route_polygon is
					use et_packages.shapes;

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
						new_item	=> (shapes.type_polygon_base (polygon) with
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
									new_item	=> (et_packages.shapes.type_line (board_line) with
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
									new_item	=> (et_packages.shapes.type_arc (board_arc) with
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
							when SEC_INIT => null;
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
				section			: in type_section_name_module) -- SEC_NETS
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
							when SEC_INIT => null; -- nothing to do
							when others => invalid_section;
						end case;

					when SEC_ASSEMBLY_VARIANT =>
						case stack.parent is
							when SEC_ASSEMBLY_VARIANTS =>
								declare
									use et_libraries;
									kw : string 	:= f (line, 1);
									device_name		: et_libraries.type_device_name; -- R1
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
										
										device_name := et_libraries.to_device_name (f (line, 2));

										-- test whether device exists
										if not exists (module_cursor, device_name) then
											log (ERROR, "device " &
												 enclose_in_quotes (et_libraries.to_string (device_name)) &
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
											device.value := et_libraries.to_value (f (line, 4));

											-- read partcode
											if f (line, 5) = keyword_partcode then
												device.partcode := material.to_partcode (f (line, 6));
											else -- keyword partcode not found
												log (ERROR, "expect keyword " & enclose_in_quotes (keyword_partcode) &
													 " after value !", console => true);
												raise constraint_error;
											end if;

											-- read optional purpose
											if field_count (line) > 6 then
												expect_field_count (line, 8);

												if f (line, 7) = keyword_purpose then

													-- validate purpose
													device.purpose := et_libraries.to_purpose (f (line, 8));

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
												 enclose_in_quotes (et_libraries.to_string (device_name)) &
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
									use et_libraries;
									kw : string := f (line, 1);
								begin
									if kw = keyword_device then -- device R1 port 1
										expect_field_count (line, 4);

										net_device_port.device_name := et_libraries.to_device_name (f (line, 2)); -- IC3

										if f (line, 3) = keyword_port then -- port
											net_device_port.port_name := et_libraries.to_port_name (f (line, 4)); -- CE

											-- Insert port in port collection of device ports. First make sure it is
											-- not already in the net segment.
											if et_schematic.type_ports_device.contains (net_device_ports, net_device_port) then
												log (ERROR, "device " & et_libraries.to_string (net_device_port.device_name) &
													" port " & et_libraries.to_string (net_device_port.port_name) & 
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

									elsif kw = keyword_size then -- size 1.3
										expect_field_count (line, 2);
										net_label.size := geometry.to_distance (f (line, 2));

									elsif kw = keyword_style then -- style normal
										expect_field_count (line, 2);
										net_label.style := et_libraries.to_text_style (f (line, 2));

									elsif kw = et_libraries.keyword_line_width then -- line_width 0.1
										expect_field_count (line, 2);
										net_label.width := et_coordinates.geometry.to_distance (f (line, 2));

									elsif kw = keyword_appearance then -- appearance tag/simple
										expect_field_count (line, 2);
										net_label_appearance := et_schematic.to_appearance (f (line, 2));

									elsif kw = keyword_direction then -- direction input/output
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

										elsif kw = keyword_width then -- width 0.5
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
												if kw = keyword_width then -- width 0.5
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
										if kw = keyword_width then -- width 0.5
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
										use et_packages.shapes;
										use et_pcb_stack;
									begin
										-- CS: In the following: set a corresponding parameter-found-flag
										if kw = keyword_layer then -- layer 2
											expect_field_count (line, 2);
											signal_layer := et_pcb_stack.to_signal_layer (f (line, 2));

										elsif kw = keyword_width then -- width 0.5
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
												if kw = keyword_width then -- width 0.5
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
										use et_packages.shapes;
										use et_pcb_stack;
									begin
										-- CS: In the following: set a corresponding parameter-found-flag
										if kw = keyword_width then -- width 0.5
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
										use et_packages.shapes;
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
												use et_packages.shapes;
												kw : string := f (line, 1);
											begin
												-- CS: In the following: set a corresponding parameter-found-flag
												if kw = keyword_width then -- circumfence line width 0.5
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
										use et_packages.shapes;
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
										use et_packages.shapes;
										use et_pcb_stack;
										use et_pcb_coordinates.geometry;
										kw : string := f (line, 1);
									begin
										-- CS: In the following: set a corresponding parameter-found-flag
										if kw = keyword_width then -- width 0.5
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
											board_hatching.line_width := to_distance (f (line, 2));

										elsif kw = keyword_hatching_line_spacing then -- hatching_line_spacing 0.3
											expect_field_count (line, 2);													
											board_hatching.spacing := to_distance (f (line, 2));
											
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
										use et_packages.shapes;
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
											use et_packages.shapes;
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
									use et_packages.shapes;
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
									use et_packages.shapes;									
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
										board_hatching.line_width := to_distance (f (line, 2));

									elsif kw = keyword_hatching_line_spacing then -- hatching_line_spacing 1
										expect_field_count (line, 2);
										board_hatching.spacing := to_distance (f (line, 2));

									elsif kw = keyword_layer then -- layer 2
										expect_field_count (line, 2);
										signal_layer := et_pcb_stack.to_signal_layer (f (line, 2));

									elsif kw = keyword_min_width then -- min_width 0.3
										expect_field_count (line, 2);
										polygon_width_min := to_distance (f (line, 2));

									elsif kw = keyword_pad_technology then -- pad_technology smt_only/tht_only/smt_and_tht
										expect_field_count (line, 2);
										thermal.technology := to_pad_technology (f (line, 2));

									elsif kw = keyword_pad_connection then -- pad_connection thermal/solid
										expect_field_count (line, 2);
										polygon_pad_connection := to_pad_connection (f (line, 2));
										
									elsif kw = keyword_thermal_width then -- thermal_width 0.3
										expect_field_count (line, 2);
										thermal.width := to_distance (f (line, 2));

									elsif kw = keyword_thermal_gap then -- thermal_gap 0.7
										expect_field_count (line, 2);
										thermal.gap := to_distance (f (line, 2));

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
											use et_packages.shapes;
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
											use et_packages.shapes;
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
									use et_packages.shapes;
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
									use et_packages.shapes;									
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
										board_hatching.line_width := to_distance (f (line, 2));

									elsif kw = keyword_hatching_line_spacing then -- hatching_line_spacing 0.3
										expect_field_count (line, 2);													
										board_hatching.spacing := to_distance (f (line, 2));

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

									elsif kw = shapes.keyword_diameter then -- diameter 0.35
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

									elsif kw = keyword_size then -- size x 30 y 30
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

											elsif kw = keyword_direction then -- direction master/slave
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
							when SEC_DRAWING_FRAMES =>
								declare
									kw : string := f (line, 1);
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_template then -- template $ET_FRAMES/drawing_frame_version_1.frm
										expect_field_count (line, 2);
										frame_template_schematic := et_libraries.to_template_name (f (line, 2));
									else
										invalid_keyword (kw);
									end if;
								end;

							when SEC_DRAWING_GRID =>
								declare
									kw : string := f (line, 1);
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_default then -- default x 1.00 y 1.00
										expect_field_count (line, 5);
										grid_schematic := to_grid (line, 2);
									else
										invalid_keyword (kw);
									end if;
								end;
								
							when others => invalid_section;
						end case;

					when SEC_BOARD =>
						case stack.parent is
							when SEC_INIT => null; -- nothing to do
							
							when SEC_DRAWING_FRAMES =>
								declare
									kw : string := f (line, 1);
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_template then -- template $ET_FRAMES/drawing_frame_version_2.frm
										expect_field_count (line, 2);
										frame_template_board := et_libraries.to_template_name (f (line, 2));
									else
										invalid_keyword (kw);
									end if;
								end;

							when SEC_DRAWING_GRID =>
								declare
									kw : string := f (line, 1);
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_default then -- default x 1.00 y 1.00
										expect_field_count (line, 5);
										grid_board := to_grid (line, 2);
									else
										invalid_keyword (kw);
									end if;
								end;
								
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
										note.content := et_libraries.to_content (f (line, 2));

									elsif kw = keyword_size then -- size 1.4
										expect_field_count (line, 2);
										note.size := to_distance (f (line, 2));

									elsif kw = et_libraries.keyword_line_width then -- line_width 0.1
										expect_field_count (line, 2);
										note.line_width := to_distance (f (line, 2));

									elsif kw = keyword_rotation then -- rotation 90
										expect_field_count (line, 2);
										note.rotation := to_rotation (f (line, 2));

									elsif kw = keyword_style then -- stlye normal/italic
										expect_field_count (line, 2);
										note.style := et_libraries.to_text_style (f (line, 2));

									elsif kw = keyword_alignment then -- alignment horizontal center vertical center
										expect_field_count (line, 5);

										-- extract alignment starting at field 2
										note.alignment := to_alignment (line, 2);
										
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

											elsif kw = keyword_size then -- size width 1.000 height 1.000
												expect_field_count (line, 5);

												-- extract text dimensions starting at field 2
												board_text.dimensions := to_dimensions (line, 2);

											elsif kw = et_packages.keyword_line_width then -- line_width 0.1
												expect_field_count (line, 2);
												board_text.line_width := to_distance (f (line, 2));

											elsif kw = keyword_alignment then -- alignment horizontal center vertical center
												expect_field_count (line, 5);

												-- extract alignment starting at field 2
												board_text.alignment := to_alignment (line, 2);
												
											elsif kw = keyword_content then -- content "WATER KETTLE CONTROL"
												expect_field_count (line, 2); -- actual content in quotes !
												board_text.content := et_libraries.to_content (f (line, 2));
												
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

									elsif kw = keyword_size then -- size width 1.000 height 1.000
										expect_field_count (line, 5);

										-- extract text dimensions starting at field 2
										board_text_copper.dimensions := to_dimensions (line, 2);

									elsif kw = et_packages.keyword_line_width then -- line_width 0.1
										expect_field_count (line, 2);
										board_text_copper.line_width := to_distance (f (line, 2));

									elsif kw = keyword_alignment then -- alignment horizontal center vertical center
										expect_field_count (line, 5);

										-- extract alignment starting at field 2
										board_text_copper.alignment := to_alignment (line, 2);
										
									elsif kw = keyword_content then -- content "TOP", "L2", "BOT"
										expect_field_count (line, 2); -- actual content in quotes !
										board_text_copper.content := et_libraries.to_content (f (line, 2));

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
									kw : string := f (line, 1);
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_name then -- name C12
										expect_field_count (line, 2);
										device_name := et_libraries.to_device_name (f (line, 2));

									-- As soon as the appearance becomes clear, a temporarily device is
									-- created where pointer "device" is pointing at:
									elsif kw = keyword_appearance then -- sch_pcb, sch
										expect_field_count (line, 2);
										device_appearance := et_libraries.to_appearance (f (line, 2));

										case device_appearance is
											when et_libraries.SCH =>
												device := new et_schematic.type_device'(
													appearance	=> et_libraries.SCH,
													others		=> <>);

											when et_libraries.SCH_PCB =>
												device := new et_schematic.type_device'(
													appearance	=> et_libraries.SCH_PCB,
													others		=> <>);
										end case;
												
									elsif kw = keyword_value then -- value 100n
										expect_field_count (line, 2);

										-- validate value
										device_value := et_libraries.to_value (f (line, 2));

									elsif kw = keyword_model then -- model /models/capacitor.dev
										expect_field_count (line, 2);
										device_model := et_libraries.to_file_name (f (line, 2));
										
									elsif kw = keyword_variant then -- variant S_0805, N, D
										expect_field_count (line, 2);
										et_libraries.check_variant_name_length (f (line, 2));
										device_variant := et_libraries.to_component_variant_name (f (line, 2));

									elsif kw = keyword_partcode then -- partcode LED_PAC_S_0805_VAL_red
										expect_field_count (line, 2);

										-- validate partcode
										device_partcode := material.to_partcode (f (line, 2));

									elsif kw = keyword_purpose then -- purpose power_out
										expect_field_count (line, 2);

										-- validate purpose
										device_purpose := et_libraries.to_purpose (f (line, 2));
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

											elsif kw = keyword_size then -- size width 3.000 height 5
												expect_field_count (line, 5);

												-- extract dimensions of placeholder text starting at field 2
												device_text_placeholder.dimensions := to_dimensions (line, 2);

											elsif kw = keyword_line_width then -- line_width 0.15
												expect_field_count (line, 2);

												device_text_placeholder.line_width := to_distance (f (line, 2));

											elsif kw = keyword_alignment then -- alignment horizontal center vertical center
												expect_field_count (line, 5);

												-- extract alignment of placeholder starting at field 2
												device_text_placeholder.alignment := to_alignment (line, 2);
												
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
												unit_placeholder_meaning := et_libraries.to_text_meaning (f (line, 2));
												
											elsif kw = keyword_position then -- position x 0.000 y 5.555
												expect_field_count (line, 5);

												-- extract position of placeholder starting at field 2
												unit_placeholder_position := to_position (line, 2);

											elsif kw = keyword_size then -- size 3.0
												expect_field_count (line, 2);

												-- extract dimensions of placeholder text starting at field 2
												unit_placeholder.size := to_distance (f (line, 2));

											elsif kw = et_libraries.keyword_line_width then -- line_width 0.15
												expect_field_count (line, 2);

												unit_placeholder.line_width := to_distance (f (line, 2));

											elsif kw = keyword_rotation then -- rotation 90.0
												expect_field_count (line, 2);

												unit_placeholder.rotation := to_rotation (f (line, 2));

											elsif kw = keyword_style then -- stlye italic
												expect_field_count (line, 2);

												unit_placeholder.style := et_libraries.to_text_style (f (line, 2));

											elsif kw = keyword_alignment then -- alignment horizontal center vertical center
												expect_field_count (line, 5);

												-- extract alignment of placeholder starting at field 2
												unit_placeholder.alignment := to_alignment (line, 2);
												
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

											elsif kw = keyword_size then -- size width 1.000 height 1.000
												expect_field_count (line, 5);

												-- extract text dimensions starting at field 2
												board_text_placeholder.dimensions := to_dimensions (line, 2);

											elsif kw = et_packages.keyword_line_width then -- line_width 0.1
												expect_field_count (line, 2);
												board_text_placeholder.line_width := to_distance (f (line, 2));

											elsif kw = keyword_alignment then -- alignment horizontal center vertical center
												expect_field_count (line, 5);

												-- extract alignment starting at field 2
												board_text_placeholder.alignment := to_alignment (line, 2);
												
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

									elsif kw = keyword_size then -- size width 1.000 height 1.000
										expect_field_count (line, 5);

										-- extract text dimensions starting at field 2
										board_text_copper_placeholder.dimensions := to_dimensions (line, 2);

									elsif kw = et_packages.keyword_line_width then -- line_width 0.1
										expect_field_count (line, 2);
										board_text_copper_placeholder.line_width := to_distance (f (line, 2));

									elsif kw = keyword_alignment then -- alignment horizontal center vertical center
										expect_field_count (line, 5);

										-- extract alignment starting at field 2
										board_text_copper_placeholder.alignment := to_alignment (line, 2);
										
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
									kw : string := f (line, 1);
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_name then -- name 1, GPIO_BANK_1, ...
										expect_field_count (line, 2);
										device_unit_name := et_libraries.to_unit_name (f (line, 2));
										
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
				log (text => "file " & file_name & latin_1.space 
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
				use type_variants;
				variant_cursor : type_variants.cursor := module.variants.first;
				variant_name : type_variant_name.bounded_string; -- low_cost

				procedure query_submodules (
					variant_name	: in type_variant_name.bounded_string;
					variant			: in type_variant) is
					use type_submodules;
					submod_cursor : type_submodules.cursor := variant.submodules.first;
					submod_name : type_module_instance_name.bounded_string; -- CLK_GENERATOR
					submod_variant : type_variant_name.bounded_string; -- fixed_frequency
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
				if variant_cursor = type_variants.no_element then
					log (text => "no variants specified", level => log_threshold);
				else
					-- iterate assembly variants of parent module
					while variant_cursor /= type_variants.no_element loop
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
						ifs 			=> latin_1.space); -- fields are separated by space

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
	
	procedure open_project (log_threshold : in et_string_processing.type_log_level) is
	-- Enters the project directory specified by project_name.
	-- Searches for rig configuration files (*.conf), reads them and stores configurations in et_project.rigs.
	-- Searches for module files (*.mod), reads them and stores modules in et_project.modules.
		use et_string_processing;
		use ada.directories;

		function f (line : in type_fields_of_line; position : in positive) return string 
			renames et_string_processing.field;
		
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
			package stack is new stack_lifo (
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
			
			purpose_A, purpose_B : et_libraries.type_device_purpose.bounded_string; -- power_in, power_out
			instance_A, instance_B : type_module_instance_name.bounded_string; -- DRV_1, PWR

			procedure clear_connector is begin
				purpose_A := et_libraries.type_device_purpose.to_bounded_string ("");
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
						use et_libraries.type_device_purpose;
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
											purpose_A := et_libraries.to_purpose (f (line,2));
											-- CS: test if a connector with this purpose exists in the instance
											
										elsif kw = keyword_purpose_B then
											expect_field_count (line, 2);
											purpose_B := et_libraries.to_purpose (f (line,2));
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
					log (text => "file " & file_name & latin_1.space & affected_line (line) 
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
					ifs 			=> latin_1.space); -- fields are separated by space

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
		
		use et_libraries.type_devices;

		procedure save_device (device_cursor : in et_libraries.type_devices.cursor) is
			use et_libraries;
		begin
			save_device (
				-- library name like: 
				-- /home/user/ecad/blood_sample_analyzer/libraries/devices/bel_connector_and_jumper_FEMALE_01X06.dev
				name	=> to_string (path) & gnat.directory_operations.dir_separator & to_string (key (device_cursor)),

				-- the device model itself:
				device	=> element (device_cursor),
				log_threshold	=> log_threshold + 1); 
		end save_device;

		use et_packages.type_packages;
		
		procedure save_package (package_cursor : in et_packages.type_packages.cursor) is
			use et_libraries.type_package_model_file;
		begin
			save_package (
				-- package name like: 
				-- /home/user/ecad/blood_sample_analyzer/libraries/packages/bel_connector_and_jumper_FEMALE_01X06.pac
				name	=> to_string (path) & gnat.directory_operations.dir_separator & to_string (key (package_cursor)),

				-- the package model itself:
				packge	=> element (package_cursor),
				log_threshold	=> log_threshold + 1); 
		end save_package;
		
	begin -- save_libraries
		log (text => "saving libraries ...", level => log_threshold);
		log_indentation_up;

		log (text => "devices ...", level => log_threshold + 1);
		log_indentation_up;
		iterate (et_libraries.devices, save_device'access);
		log_indentation_down;
		
		log (text => "packages ...", level => log_threshold + 1);
		log_indentation_up;
		iterate (et_packages.packages, save_package'access);
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
		device	: in et_libraries.type_device_name)
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
				if type_variants.contains (submodule.variants, variant) then
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

		use assembly_variants;
		use type_variants;

		result : boolean := false; -- to be returned

		procedure query_variants (
			module_name	: in type_module_name.bounded_string;
			module		: in et_schematic.type_module) is
		begin
			result := contains (module.variants, variant);
		end;
		
	begin -- exists
		if type_variant_name.length (variant) = 0 then
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
		device	: in et_libraries.type_device_name)
		return boolean is

		result : boolean := false; -- to be returned

		procedure query_variants (
			module_name	: in type_module_name.bounded_string;
			module		: in et_schematic.type_module) is
			use assembly_variants;
			use type_variants;
			variant_cursor : type_variants.cursor;

			procedure query_devices (
				variant_name	: in type_variant_name.bounded_string;
				variant			: in type_variant) is
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
		device	: in et_libraries.type_device_name)
		return assembly_variants.type_devices.cursor is

		cursor : assembly_variants.type_devices.cursor; -- to be returned;
		
		procedure query_variants (
			module_name	: in type_module_name.bounded_string;
			module		: in et_schematic.type_module) is
			use assembly_variants;
			use type_variants;
			variant_cursor : type_variants.cursor;

			procedure query_devices (
				variant_name	: in type_variant_name.bounded_string;
				variant			: in type_variant) is
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
			use assembly_variants;
			use type_variants;
			variant_cursor : type_variants.cursor;

			procedure query_submodules (
				variant_name	: in type_variant_name.bounded_string;
				variant			: in type_variant) is
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
	
-- GENERICS
	
	package body stack_lifo is
		s : array (1..max) of item;
		top : natural range 0..max;

		procedure push (x : in item) is
		begin
			top := top + 1;
			s (top) := x;
		end push;

		procedure pop is
		begin
			top := top - 1;
		end pop;
		
		function pop return item is
		begin
			top := top - 1;
			return s (top + 1);
		end pop;

		function depth return natural is
		begin
			return top;
		end depth;

		procedure init is
		begin
			top := 0;
		end init;

		function empty return boolean is
		begin
			if top = 0 then return true;
			else return false;
			end if;
		end empty;
		
		function current return item is 
		begin
			return s (top);
		end current;
		
		function parent (degree : in natural := 1) return item is
		begin
			--return s (top - 1);
			return s (top - degree);
		end parent;
		
	end stack_lifo;

end et_project;
	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
