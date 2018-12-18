------------------------------------------------------------------------------
--                                                                          --
--                         SYSTEM ET PROJECT                                --
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

with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;
with ada.strings; 				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;

with ada.text_io;				use ada.text_io;

with ada.tags;

with ada.exceptions;
with ada.directories;

with ada.containers;            use ada.containers;
with ada.containers.ordered_maps;

with et_general;
with et_coordinates;
with et_string_processing;
with et_libraries;
with et_export;
with et_import;
with et_schematic;
with et_pcb;
with et_pcb_coordinates;

package body et_project is

	function to_string (project_name : in type_project_name.bounded_string) return string is
	begin
		return type_project_name.to_string (project_name);
	end to_string;
	
	function to_project_name (name : in string) return type_project_name.bounded_string is
	-- Converts the given string to type_project_name.
	begin
		return type_project_name.to_bounded_string (name);
	end to_project_name;

	function to_string (path : in type_et_project_path.bounded_string) return string is
	begin
		return type_et_project_path.to_string (path);
	end to_string;

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
		use et_libraries.type_component_purpose;
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
	
	procedure create_project_directory (
	-- Creates given project directory in the given project_path.
	-- Already existing projects in given project_path are overwritten.
	-- Sets the global project file name so that subsequent write and read operations
	-- know the right project file.
	-- Leaves the project file (global project_file_handle) open (closes it on exception).
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
			--log ("subdir " & compose (path, directory_libraries_devices));
		end create_library_subdirs;

		procedure create_rig_configuration is
		-- create the rig configuration file
			file_handle : ada.text_io.file_type;
			rig_conf_file : type_rig_configuration_file_name.bounded_string; -- led_matrix.conf
		begin
			log ("creating the rig configuration file ...", log_threshold + 1);
			rig_conf_file := type_rig_configuration_file_name.to_bounded_string (compose (
				containing_directory	=> to_string (path),
				name 					=> to_string (project_name),
				extension 				=> rig_configuration_file_extension));

			create (
				file => file_handle,
				mode => out_file, 
				name => type_rig_configuration_file_name.to_string (rig_conf_file));

			set_output (file_handle);

			-- write a nice header
			put_line (comment_mark & " " & system_name & " rig configuration file");
			put_line (comment_mark & " " & date);
			put_line (comment_mark & " " & row_separator_double);
			new_line;

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

			
			-- write a nice footer
			new_line;
			put_line (comment_mark & " " & row_separator_double);
			put_line (comment_mark & " " & date);
			put_line (comment_mark & " rig configuration file end");
			new_line;
			
			set_output (standard_output);
			close (file_handle);
		end create_rig_configuration;

	begin -- create_project_directory
		log ("creating native project " & to_string (path) & " ...", log_threshold);
		log_indentation_up;
		
		-- delete previous project directory
		if exists (to_string (path)) then
			delete_tree (to_string (path));
		end if;
		
		-- create project root directory
		create_path (to_string (path));
		
		-- create sub-directories for supplementary stuff:
		log ("creating subdirectories for supplementary stuff ...", log_threshold + 1);
		create_directory (compose (to_string (path), directory_libraries));
		create_library_subdirs (compose (to_string (path), directory_libraries));
		
		create_directory (compose (to_string (path), directory_dru));
		create_directory (compose (to_string (path), directory_cam));
		create_directory (compose (to_string (path), directory_net_classes));
		create_directory (compose (to_string (path), directory_settings));
		create_directory (compose (to_string (path), directory_reports));
		create_directory (compose (to_string (path), directory_documentation));
		create_directory (compose (to_string (path), directory_miscellaneous));

		create_rig_configuration;

		log_indentation_down;
		
		exception when event:
			others => 
				log (ada.exceptions.exception_message (event), console => true);
				raise;
		
	end create_project_directory;




	function position (pos : in et_coordinates.type_2d_point'class) return string is
	-- Returns something like "x 12.34 y 45.0" or "sheet 3 x 12.34 y 45.0".
	-- This kind of output depends on the tag of the given object.
		use et_coordinates;
		use ada.tags;

		-- This function returns the basic text with x and y coordinates.
		function text return string is begin return 
			space & keyword_pos_x & to_string (distance_x (pos)) 
			& space & keyword_pos_y & to_string (distance_y (pos));
		end text;
		
	begin -- position
		if pos'tag = type_2d_point'tag then
			return text; -- a 2d point has just x and y
		else
			-- A type_coordinates also has the sheet number:
			return space & keyword_sheet & to_string (sheet (type_coordinates (pos))) & text;
		end if;
	end position;

	function rotation (angle : in et_coordinates.type_angle) return string is begin
		return type_angle'image (angle);
	end rotation;


	function position (point : et_pcb_coordinates.type_point_2d'class) return string is
		use et_pcb_coordinates;
	begin
		return space & keyword_pos_x & to_string (get_axis (X, point)) 
			& space & keyword_pos_y & to_string (get_axis (Y, point));
	end position;
	
	procedure write_text_properties (text : in et_libraries.type_text_basic'class) is
		use et_coordinates;
	begin
		write (keyword => keyword_size, parameters => et_libraries.to_string (text.size, preamble => false));
		write (keyword => keyword_line_width, parameters => to_string (text.line_width));
		write (keyword => keyword_rotation, parameters => rotation (text.orientation));
		write (keyword => keyword_style, parameters => et_libraries.to_string (text.style));
		write (keyword => keyword_alignment, parameters => space &
				keyword_horizontal & et_libraries.to_string (text.alignment.horizontal) & space &
				keyword_vertical   & et_libraries.to_string (text.alignment.vertical)
				);
		--write (keyword => keyword_hidden, parameters => et_libraries.to_string (text.visible)); -- CS: no need. probably useless
	end write_text_properties;

	procedure write_text_properties (text : in et_pcb.type_text'class) is
		use et_pcb_coordinates;
	begin
		write (keyword => keyword_position, parameters => position (text.position));
		write (keyword => keyword_size, parameters => space & keyword_pos_x & to_string (text.size_x) 
				& space & keyword_pos_y & to_string (text.size_y));
		write (keyword => keyword_line_width, parameters => to_string (text.width));
		write (keyword => keyword_rotation, parameters => to_string (text.angle));
		write (keyword => keyword_alignment, parameters => space &
				keyword_horizontal & et_libraries.to_string (text.alignment.horizontal) & space &
				keyword_vertical   & et_libraries.to_string (text.alignment.vertical)
				);
		write (keyword => keyword_hidden, parameters => space & to_lower (boolean'image (text.hidden)));
	end write_text_properties;

	procedure line_begin is begin section_mark (section_line, HEADER); end;
	procedure line_end   is begin section_mark (section_line, FOOTER); end;			
	procedure arc_begin  is begin section_mark (section_arc , HEADER); end;
	procedure arc_end    is begin section_mark (section_arc , FOOTER); end;
	procedure circle_begin is begin section_mark (section_circle, HEADER); end;
	procedure circle_end   is begin section_mark (section_circle, FOOTER); end;			
	procedure polygon_begin is begin section_mark (section_polygon, HEADER); end;
	procedure polygon_end   is begin section_mark (section_polygon, FOOTER); end;
	procedure corners_begin is begin section_mark (section_corners, HEADER); end;
	procedure corners_end   is begin section_mark (section_corners, FOOTER); end;
	procedure text_begin is begin section_mark (section_text, HEADER); end;
	procedure text_end   is begin section_mark (section_text, FOOTER); end;
	procedure placeholder_begin is begin section_mark (section_placeholder, HEADER); end;
	procedure placeholder_end   is begin section_mark (section_placeholder, FOOTER); end;
	
	procedure write_text (cursor : in et_pcb.type_texts_with_content.cursor) is
		use et_pcb.type_texts_with_content;
	begin
		text_begin;
		write (keyword => keyword_content, parameters => et_libraries.to_string (element (cursor).content));
		write_text_properties (element (cursor));
		text_end;
	end write_text;
	
	procedure write_polygon_corners (pc : in et_pcb.type_polygon_points.cursor) is
		use et_pcb.type_polygon_points;
	begin
		write (keyword => keyword_position, parameters => position (element (pc)));
	end write_polygon_corners;

-- KEEPOUT
	procedure write_line (cursor : in et_pcb.type_keepout_lines.cursor) is
		use et_pcb.type_keepout_lines;
		use et_pcb_coordinates;		
	begin
		line_begin;
		write (keyword => keyword_start, parameters => position (element (cursor).start_point));
		write (keyword => keyword_end  , parameters => position (element (cursor).end_point));
		write (keyword => keyword_width, parameters => to_string (element (cursor).width));
		line_end;
	end write_line;

	procedure write_arc (cursor : in et_pcb.type_keepout_arcs.cursor) is 
		use et_pcb.type_keepout_arcs;
		use et_pcb_coordinates;		
	begin
		arc_begin;
		write (keyword => keyword_center, parameters => position (element (cursor).center));
		write (keyword => keyword_start, parameters => position (element (cursor).start_point));
		write (keyword => keyword_end  , parameters => position (element (cursor).end_point));
		write (keyword => keyword_width, parameters => to_string (element (cursor).width));
		arc_end;
	end write_arc;

	procedure write_circle (cursor : in et_pcb.type_keepout_circles.cursor) is 
		use et_pcb;
		use type_keepout_circles;
		use et_pcb_coordinates;
	begin
		circle_begin;
		write (keyword => keyword_center, parameters => position (element (cursor).center));
		write (keyword => keyword_radius, parameters => to_string (element (cursor).radius));
		write (keyword => keyword_width , parameters => to_string (element (cursor).width));
		write (keyword => keyword_filled, parameters => to_string (element (cursor).filled));
		write (keyword => keyword_fill_style, parameters => to_string (element (cursor).fill_style));
		write (keyword => keyword_hatching_line_width  , parameters => to_string (element (cursor).hatching_line_width));
		write (keyword => keyword_hatching_line_spacing, parameters => to_string (element (cursor).hatching_spacing));
		circle_end;
	end write_circle;
	
	procedure write_polygon (cursor : in et_pcb.type_keepout_polygons.cursor) is 
		use et_pcb;		
		use type_keepout_polygons;
		use type_polygon_points;
		use et_pcb_coordinates;
		
		procedure query_points (polygon : in type_keepout_polygon) is begin
			iterate (polygon.points, write_polygon_corners'access); -- see general stuff above
		end query_points;
		
	begin -- write_polygon
		polygon_begin;
		write (keyword => keyword_fill_style, parameters => to_string (element (cursor).fill_style));
		write (keyword => keyword_hatching_line_width  , parameters => to_string (element (cursor).hatching_line_width));
		write (keyword => keyword_hatching_line_spacing, parameters => to_string (element (cursor).hatching_spacing));
		write (keyword => keyword_corner_easing, parameters => to_string (element (cursor).corner_easing));
		write (keyword => keyword_easing_radius, parameters => to_string (element (cursor).easing_radius));
		corners_begin;
		query_element (cursor, query_points'access);
		corners_end;
		polygon_end;
	end write_polygon;
	
-- STOP MASK
	procedure write_line (cursor : in et_pcb.type_stop_lines.cursor) is 
		use et_pcb;
		use type_stop_lines;
		use et_pcb_coordinates;
	begin
		line_begin;
		write (keyword => keyword_start, parameters => position (element (cursor).start_point));
		write (keyword => keyword_end  , parameters => position (element (cursor).end_point));
		write (keyword => keyword_width, parameters => to_string (element (cursor).width));
		line_end;
	end write_line;

	procedure write_arc (cursor : in et_pcb.type_stop_arcs.cursor) is 
		use et_pcb;
		use type_stop_arcs;
		use et_pcb_coordinates;
	begin
		arc_begin;
		write (keyword => keyword_center, parameters => position (element (cursor).center));
		write (keyword => keyword_start, parameters => position (element (cursor).start_point));
		write (keyword => keyword_end  , parameters => position (element (cursor).end_point));
		write (keyword => keyword_width, parameters => to_string (element (cursor).width));
		arc_end;
	end write_arc;

	procedure write_circle (cursor : in et_pcb.type_stop_circles.cursor) is 
		use et_pcb;
		use type_stop_circles;
		use et_pcb_coordinates;
	begin
		circle_begin;
		write (keyword => keyword_center, parameters => position (element (cursor).center));
		write (keyword => keyword_radius, parameters => to_string (element (cursor).radius));
		write (keyword => keyword_width , parameters => to_string (element (cursor).width));
		write (keyword => keyword_filled, parameters => to_string (element (cursor).filled));
		write (keyword => keyword_fill_style, parameters => to_string (element (cursor).fill_style));
		write (keyword => keyword_hatching_line_width  , parameters => to_string (element (cursor).hatching_line_width));
		write (keyword => keyword_hatching_line_spacing, parameters => to_string (element (cursor).hatching_spacing));
		circle_end;
	end write_circle;
	
	procedure write_polygon (cursor : in et_pcb.type_stop_polygons.cursor) is 
		use et_pcb;
		use type_stop_polygons;
		use et_pcb_coordinates;
		use type_polygon_points;
		
		procedure query_points (polygon : in type_stop_polygon) is begin
			iterate (polygon.points, write_polygon_corners'access); -- see general stuff above
		end query_points;
		
	begin -- write_polygon
		polygon_begin;
		write (keyword => keyword_fill_style, parameters => to_string (element (cursor).fill_style));
		write (keyword => keyword_hatching_line_width  , parameters => to_string (element (cursor).hatching_line_width));
		write (keyword => keyword_hatching_line_spacing, parameters => to_string (element (cursor).hatching_spacing));
		write (keyword => keyword_corner_easing, parameters => to_string (element (cursor).corner_easing));
		write (keyword => keyword_easing_radius, parameters => to_string (element (cursor).easing_radius));
		corners_begin;
		query_element (cursor, query_points'access);
		corners_end;
		polygon_end;
	end write_polygon;

-- STENCIL (OR SOLDER PASTE MASK)
	procedure write_line (cursor : in et_pcb.type_stencil_lines.cursor) is 
		use et_pcb;
		use type_stencil_lines;
		use et_pcb_coordinates;
	begin
		line_begin;
		write (keyword => keyword_start, parameters => position (element (cursor).start_point));
		write (keyword => keyword_end  , parameters => position (element (cursor).end_point));
		write (keyword => keyword_width, parameters => to_string (element (cursor).width));
		line_end;
	end write_line;

	procedure write_arc (cursor : in et_pcb.type_stencil_arcs.cursor) is 
		use et_pcb;
		use type_stencil_arcs;
		use et_pcb_coordinates;
	begin
		arc_begin;
		write (keyword => keyword_center, parameters => position (element (cursor).center));
		write (keyword => keyword_start, parameters => position (element (cursor).start_point));
		write (keyword => keyword_end  , parameters => position (element (cursor).end_point));
		write (keyword => keyword_width, parameters => to_string (element (cursor).width));
		arc_end;
	end write_arc;

	procedure write_circle (cursor : in et_pcb.type_stencil_circles.cursor) is 
		use et_pcb;
		use type_stencil_circles;
		use et_pcb_coordinates;
	begin
		circle_begin;
		write (keyword => keyword_center, parameters => position (element (cursor).center));
		write (keyword => keyword_radius, parameters => to_string (element (cursor).radius));
		write (keyword => keyword_width , parameters => to_string (element (cursor).width));
		write (keyword => keyword_filled, parameters => to_string (element (cursor).filled));
		write (keyword => keyword_fill_style, parameters => to_string (element (cursor).fill_style));
		write (keyword => keyword_hatching_line_width  , parameters => to_string (element (cursor).hatching_line_width));
		write (keyword => keyword_hatching_line_spacing, parameters => to_string (element (cursor).hatching_spacing));
		circle_end;
	end write_circle;
	
	procedure write_polygon (cursor : in et_pcb.type_stencil_polygons.cursor) is 
		use et_pcb;
		use type_stencil_polygons;
		use et_pcb_coordinates;		
		use type_polygon_points;
		
		procedure query_points (polygon : in type_stencil_polygon) is begin
			iterate (polygon.points, write_polygon_corners'access); -- see general stuff above
		end query_points;
		
	begin -- write_polygon
		polygon_begin;
		write (keyword => keyword_fill_style, parameters => to_string (element (cursor).fill_style));
		write (keyword => keyword_hatching_line_width  , parameters => to_string (element (cursor).hatching_line_width));
		write (keyword => keyword_hatching_line_spacing, parameters => to_string (element (cursor).hatching_spacing));
		write (keyword => keyword_corner_easing, parameters => to_string (element (cursor).corner_easing));
		write (keyword => keyword_easing_radius, parameters => to_string (element (cursor).easing_radius));
		corners_begin;
		query_element (cursor, query_points'access);
		corners_end;
		polygon_end;
	end write_polygon;

	procedure write_layer_numbers (layers : in et_pcb.type_signal_layers.set) is
		use et_pcb;
		use type_signal_layers;
		-- For each layer number we require 3 characters (like "10 12 13 ...")
		-- CS the count should be set in a more professional way.
		-- CS Constraint error will be raised if layer numbers assume 3 digits (some day...)
		count : count_type := length (layers) * 3; 

		-- The layer numbers will be stored here:
		package type_layers is new generic_bounded_length (positive (count)); use type_layers;
		layers_string : type_layers.bounded_string;
		
		procedure read_them (slc : in type_signal_layers.cursor) is
		begin
			layers_string := layers_string & to_bounded_string (to_string (element (slc)));
		end read_them;
		
	begin -- write_layer_numbers
		iterate (layers, read_them'access);
		write (keyword => keyword_layers, parameters => to_string (layers_string));
	end write_layer_numbers;

	
-- ROUTE RESTRICT
	procedure write_line (cursor : in et_pcb.type_route_restrict_lines.cursor) is 
		use et_pcb;
		use type_route_restrict_lines;
		use et_pcb_coordinates;		
	begin
		line_begin;
		write (keyword => keyword_start, parameters => position (element (cursor).start_point));
		write (keyword => keyword_end  , parameters => position (element (cursor).end_point));
		write (keyword => keyword_width, parameters => to_string (element (cursor).width));
		write_layer_numbers (element (cursor).layers);
		line_end;
	end write_line;

	procedure write_arc (cursor : in et_pcb.type_route_restrict_arcs.cursor) is 
		use et_pcb;
		use type_route_restrict_arcs;
		use et_pcb_coordinates;		
	begin
		arc_begin;
		write (keyword => keyword_center, parameters => position (element (cursor).center));
		write (keyword => keyword_start, parameters => position (element (cursor).start_point));
		write (keyword => keyword_end  , parameters => position (element (cursor).end_point));
		write (keyword => keyword_width, parameters => to_string (element (cursor).width));
		write_layer_numbers (element (cursor).layers);
		arc_end;
	end write_arc;

	procedure write_circle (cursor : in et_pcb.type_route_restrict_circles.cursor) is 
		use et_pcb;
		use type_route_restrict_circles;
		use et_pcb_coordinates;		
	begin
		circle_begin;
		write (keyword => keyword_center, parameters => position (element (cursor).center));
		write (keyword => keyword_radius, parameters => to_string (element (cursor).radius));
		write (keyword => keyword_width , parameters => to_string (element (cursor).width));
		write_layer_numbers (element (cursor).layers);
		circle_end;
	end write_circle;
	
	procedure write_polygon (cursor : in et_pcb.type_route_restrict_polygons.cursor) is 
		use et_pcb;
		use type_route_restrict_polygons;
		use et_pcb_coordinates;		
		use type_polygon_points;
		
		procedure query_points (polygon : in type_route_restrict_polygon) is begin
			iterate (polygon.points, write_polygon_corners'access); -- see general stuff above
		end query_points;
		
	begin -- write_polygon
		polygon_begin;
		write (keyword => keyword_fill_style, parameters => to_string (element (cursor).fill_style));
		write (keyword => keyword_hatching_line_width  , parameters => to_string (element (cursor).hatching_line_width));
		write (keyword => keyword_hatching_line_spacing, parameters => to_string (element (cursor).hatching_spacing));
		write (keyword => keyword_corner_easing, parameters => to_string (element (cursor).corner_easing));
		write (keyword => keyword_easing_radius, parameters => to_string (element (cursor).easing_radius));
		-- CS write_layer_numbers (element (cursor).layers);
		corners_begin;
		query_element (cursor, query_points'access);
		corners_end;
		polygon_end;
	end write_polygon;

-- VIA RESTRICT
	procedure write_line (cursor : in et_pcb.type_via_restrict_lines.cursor) is 
		use et_pcb;
		use type_via_restrict_lines;
		use et_pcb_coordinates;		
	begin
		line_begin;
		write (keyword => keyword_start, parameters => position (element (cursor).start_point));
		write (keyword => keyword_end  , parameters => position (element (cursor).end_point));
		write (keyword => keyword_width, parameters => to_string (element (cursor).width));
		write_layer_numbers (element (cursor).layers);
		line_end;
	end write_line;

	procedure write_arc (cursor : in et_pcb.type_via_restrict_arcs.cursor) is 
		use et_pcb;
		use type_via_restrict_arcs;
		use et_pcb_coordinates;		
	begin
		arc_begin;
		write (keyword => keyword_center, parameters => position (element (cursor).center));
		write (keyword => keyword_start, parameters => position (element (cursor).start_point));
		write (keyword => keyword_end  , parameters => position (element (cursor).end_point));
		write (keyword => keyword_width, parameters => to_string (element (cursor).width));
		write_layer_numbers (element (cursor).layers);
		arc_end;
	end write_arc;

	procedure write_circle (cursor : in et_pcb.type_via_restrict_circles.cursor) is 
		use et_pcb;
		use type_via_restrict_circles;
		use et_pcb_coordinates;		
	begin
		circle_begin;
		write (keyword => keyword_center, parameters => position (element (cursor).center));
		write (keyword => keyword_radius, parameters => to_string (element (cursor).radius));
		write (keyword => keyword_width , parameters => to_string (element (cursor).width));
		write_layer_numbers (element (cursor).layers);
		circle_end;
	end write_circle;
	
	procedure write_polygon (cursor : in et_pcb.type_via_restrict_polygons.cursor) is 
		use et_pcb;
		use type_via_restrict_polygons;
		use et_pcb_coordinates;		
		use type_polygon_points;
		
		procedure query_points (polygon : in type_via_restrict_polygon) is begin
			iterate (polygon.points, write_polygon_corners'access); -- see general stuff above
		end query_points;
		
	begin -- write_polygon
		polygon_begin;
		write (keyword => keyword_fill_style, parameters => to_string (element (cursor).fill_style));
		write (keyword => keyword_hatching_line_width  , parameters => to_string (element (cursor).hatching_line_width));
		write (keyword => keyword_hatching_line_spacing, parameters => to_string (element (cursor).hatching_spacing));
		write (keyword => keyword_corner_easing, parameters => to_string (element (cursor).corner_easing));
		write (keyword => keyword_easing_radius, parameters => to_string (element (cursor).easing_radius));
		-- CS write_layer_numbers (element (cursor).layers);
		corners_begin;
		query_element (cursor, query_points'access);
		corners_end;
		polygon_end;
	end write_polygon;

	
-- BOARD CONTOUR
	procedure write_line (cursor : in et_pcb.type_pcb_contour_lines.cursor) is 
		use et_pcb;
		use type_pcb_contour_lines;
		use et_pcb_coordinates;		
	begin
		line_begin;
		write (keyword => keyword_start , parameters => position (element (cursor).start_point));
		write (keyword => keyword_end   , parameters => position (element (cursor).end_point));
		write (keyword => keyword_locked, parameters => to_string (element (cursor).locked));
		line_end;
	end write_line;

	procedure write_arc (cursor : in et_pcb.type_pcb_contour_arcs.cursor) is 
		use et_pcb;
		use type_pcb_contour_arcs;
		use et_pcb_coordinates;		
	begin
		arc_begin;
		write (keyword => keyword_center, parameters => position (element (cursor).center));
		write (keyword => keyword_start, parameters => position (element (cursor).start_point));
		write (keyword => keyword_end  , parameters => position (element (cursor).end_point));
		write (keyword => keyword_locked, parameters => to_string (element (cursor).locked));
		arc_end;
	end write_arc;

	procedure write_circle (cursor : in et_pcb.type_pcb_contour_circles.cursor) is 
		use et_pcb;
		use type_pcb_contour_circles;
		use et_pcb_coordinates;		
	begin
		circle_begin;
		write (keyword => keyword_center, parameters => position (element (cursor).center));
		write (keyword => keyword_radius, parameters => to_string (element (cursor).radius));
		write (keyword => keyword_locked, parameters => to_string (element (cursor).locked));
		circle_end;
	end write_circle;

-- SILK SCREEN
	procedure write_line (cursor : in et_pcb.type_silk_lines.cursor) is 
		use et_pcb;
		use type_silk_lines;
		use et_pcb_coordinates;		
	begin
		line_begin;
		write (keyword => keyword_start, parameters => position (element (cursor).start_point));
		write (keyword => keyword_end  , parameters => position (element (cursor).end_point));
		write (keyword => keyword_width, parameters => to_string (element (cursor).width));
		line_end;
	end write_line;

	procedure write_arc (cursor : in et_pcb.type_silk_arcs.cursor) is 
		use et_pcb;
		use type_silk_arcs;
		use et_pcb_coordinates;		
	begin
		arc_begin;
		write (keyword => keyword_center, parameters => position (element (cursor).center));
		write (keyword => keyword_start, parameters => position (element (cursor).start_point));
		write (keyword => keyword_end  , parameters => position (element (cursor).end_point));
		write (keyword => keyword_width, parameters => to_string (element (cursor).width));
		arc_end;
	end write_arc;

	procedure write_circle (cursor : in et_pcb.type_silk_circles.cursor) is 
		use et_pcb;
		use type_silk_circles;
		use et_pcb_coordinates;		
	begin
		circle_begin;
		write (keyword => keyword_center, parameters => position (element (cursor).center));
		write (keyword => keyword_radius, parameters => to_string (element (cursor).radius));
		write (keyword => keyword_width , parameters => to_string (element (cursor).width));
		write (keyword => keyword_filled, parameters => to_string (element (cursor).filled));
		write (keyword => keyword_fill_style, parameters => to_string (element (cursor).fill_style));
		write (keyword => keyword_hatching_line_width  , parameters => to_string (element (cursor).hatching_line_width));
		write (keyword => keyword_hatching_line_spacing, parameters => to_string (element (cursor).hatching_spacing));
		circle_end;
	end write_circle;
	
	procedure write_polygon (cursor : in et_pcb.type_silk_polygons.cursor) is 
		use et_pcb;
		use type_silk_polygons;
		use et_pcb_coordinates;		
		use type_polygon_points;
		
		procedure query_points (polygon : in type_silk_polygon) is begin
			iterate (polygon.points, write_polygon_corners'access); -- see general stuff above
		end query_points;
		
	begin -- write_polygon
		polygon_begin;
		write (keyword => keyword_fill_style, parameters => to_string (element (cursor).fill_style));
		write (keyword => keyword_hatching_line_width  , parameters => to_string (element (cursor).hatching_line_width));
		write (keyword => keyword_hatching_line_spacing, parameters => to_string (element (cursor).hatching_spacing));
		write (keyword => keyword_corner_easing, parameters => to_string (element (cursor).corner_easing));
		write (keyword => keyword_easing_radius, parameters => to_string (element (cursor).easing_radius));
		corners_begin;
		query_element (cursor, query_points'access);
		corners_end;
		polygon_end;
	end write_polygon;

-- ASSEMBLY DOCUMENTATION
	procedure write_line (cursor : in et_pcb.type_doc_lines.cursor) is 
		use et_pcb;
		use type_doc_lines;
		use et_pcb_coordinates;		
	begin
		line_begin;
		write (keyword => keyword_start, parameters => position (element (cursor).start_point));
		write (keyword => keyword_end  , parameters => position (element (cursor).end_point));
		write (keyword => keyword_width, parameters => to_string (element (cursor).width));
		line_end;
	end write_line;

	procedure write_arc (cursor : in et_pcb.type_doc_arcs.cursor) is 
		use et_pcb;
		use type_doc_arcs;
		use et_pcb_coordinates;		
	begin
		arc_begin;
		write (keyword => keyword_center, parameters => position (element (cursor).center));
		write (keyword => keyword_start, parameters => position (element (cursor).start_point));
		write (keyword => keyword_end  , parameters => position (element (cursor).end_point));
		write (keyword => keyword_width, parameters => to_string (element (cursor).width));
		arc_end;
	end write_arc;

	procedure write_circle (cursor : in et_pcb.type_doc_circles.cursor) is
		use et_pcb;
		use type_doc_circles;
		use et_pcb_coordinates;		
	begin
		circle_begin;
		write (keyword => keyword_center, parameters => position (element (cursor).center));
		write (keyword => keyword_radius, parameters => to_string (element (cursor).radius));
		write (keyword => keyword_width , parameters => to_string (element (cursor).width));
		write (keyword => keyword_filled, parameters => to_string (element (cursor).filled));
		write (keyword => keyword_fill_style, parameters => to_string (element (cursor).fill_style));
		write (keyword => keyword_hatching_line_width  , parameters => to_string (element (cursor).hatching_line_width));
		write (keyword => keyword_hatching_line_spacing, parameters => to_string (element (cursor).hatching_spacing));
		circle_end;
	end write_circle;
	
	procedure write_polygon (cursor : in et_pcb.type_doc_polygons.cursor) is 
		use et_pcb;
		use type_doc_polygons;
		use et_pcb_coordinates;		
		use type_polygon_points;
		
		procedure query_points (polygon : in type_doc_polygon) is begin
			iterate (polygon.points, write_polygon_corners'access); -- see general stuff above
		end query_points;
		
	begin -- write_polygon
		polygon_begin;
		write (keyword => keyword_fill_style, parameters => to_string (element (cursor).fill_style));
		write (keyword => keyword_hatching_line_width  , parameters => to_string (element (cursor).hatching_line_width));
		write (keyword => keyword_hatching_line_spacing, parameters => to_string (element (cursor).hatching_spacing));
		write (keyword => keyword_corner_easing, parameters => to_string (element (cursor).corner_easing));
		write (keyword => keyword_easing_radius, parameters => to_string (element (cursor).easing_radius));
		corners_begin;
		query_element (cursor, query_points'access);
		corners_end;
		polygon_end;
	end write_polygon;

	
	
	procedure save_module (
		project_name	: in type_project_name.bounded_string;		-- blood_sample_analyzer
		module_name		: in type_submodule_name.bounded_string := type_submodule_name.to_bounded_string ("");	-- motor_driver
		project_path	: in type_et_project_path.bounded_string; 	-- /home/user/et_projects
		log_threshold 	: in et_string_processing.type_log_level) is
	-- Saves the schematic and layout data (stored in et_schematic.module) in the module file of the given project.
	-- If module_name not provided, the module will be named after the given project_name.
	-- CS: improve log messages !!
		
		use et_string_processing;
		use et_schematic;

		module_file_handle : ada.text_io.file_type;
		
		procedure write_module_header is 
		-- Creates the module file and writes a nice header in it.
			use ada.directories;
			use type_project_name;
			use type_et_project_path;
			use et_general;

			package type_path is new generic_bounded_length (project_name_max + project_path_max + 1); -- incl. directory separator
			use type_path;
			path : type_path.bounded_string := to_bounded_string (compose (to_string (project_path), to_string (project_name)));

			module_file_name : type_module_file_name.bounded_string; -- led_matrix
			
		begin -- write_module_header
			log ("setting module file name ...", log_threshold + 1);

			-- If given module_name is empty (means it has not been passed), the module is named after the project.
			-- Otherwise the module name is as given by module_name.
			if type_submodule_name.length (module_name) = 0 then
				module_file_name := type_module_file_name.to_bounded_string (compose (
					containing_directory	=> to_string (path),
					name 					=> to_string (project_name),
					extension 				=> module_file_name_extension));
			else
				module_file_name := type_module_file_name.to_bounded_string (compose (
					containing_directory	=> to_string (path),
					name 					=> to_string (module_name),
					extension 				=> module_file_name_extension));
			end if;
			
			log (" module file name is now " & type_module_file_name.to_string (module_file_name), log_threshold + 2);

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
		end write_module_header;
		
		procedure write_module_footer is
		-- writes a nice footer in the module file and closes it.
		begin
			new_line;
			
			log ("closing module file ...", log_threshold + 1);
			
			put_line (comment_mark & " " & row_separator_double);
			put_line (comment_mark & " " & date);
			put_line (comment_mark & " module file end");
			new_line;

			set_output (standard_output);		
			close (module_file_handle);
		end write_module_footer;
		
		function rotation (pos : in et_pcb_coordinates.type_terminal_position'class) return string is
			use et_pcb_coordinates;
		begin
			return to_string (get_angle (pos));
		end rotation;
		
		function face (point : et_pcb_coordinates.type_package_position) return string is
			use et_pcb_coordinates;
		begin
			return to_string (get_face (point));
		end face;
		
		procedure query_net_classes is
			use et_pcb;
			use et_pcb.type_net_classes;
			class_cursor : et_pcb.type_net_classes.cursor := et_schematic.module.net_classes.first;

			use et_pcb_coordinates;
		begin
			log_indentation_up;
			section_mark (section_net_classes, HEADER);
			while class_cursor /= type_net_classes.no_element loop
				log ("net class " & to_string (key (class_cursor)), log_threshold + 1);
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
				next (class_cursor);
			end loop;
			section_mark (section_net_classes, FOOTER);
			log_indentation_down;
		end query_net_classes;

		procedure query_nets is
			use et_schematic;
			use et_schematic.type_nets;
			net_cursor : et_schematic.type_nets.cursor := et_schematic.module.nets.first;

			use et_pcb;

			procedure query_strands (net_name : in type_net_name.bounded_string; net : in type_net) is
				use type_strands;
				strand_cursor : type_strands.cursor := net.strands.first;

				procedure query_segments (strand : in type_strand) is
					use type_net_segments;
					segment_cursor : type_net_segments.cursor := strand.segments.first;

					procedure query_labels (segment : in type_net_segment) is
						use type_net_labels;
						label_cursor : type_net_labels.cursor := segment.labels.first;
						use et_libraries;
					begin -- query_labels
						section_mark (section_labels, HEADER);
						while label_cursor /= type_net_labels.no_element loop
							section_mark (section_label, HEADER);
							
							write (keyword => keyword_position, parameters => position (element (label_cursor).coordinates));
							write (keyword => keyword_rotation, parameters => rotation (element (label_cursor).orientation));
							write (keyword => keyword_size, parameters => 
								   et_libraries.to_string (size => element (label_cursor).size, preamble => false));
							write (keyword => keyword_style, parameters => to_string (element (label_cursor).style));
							write (keyword => keyword_line_width, parameters =>
								   et_libraries.to_string (width => element (label_cursor).width));

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
					end query_labels;

					procedure query_junctions (segment : in type_net_segment) is
						use type_junctions;
						junction_cursor : type_junctions.cursor := segment.junctions.first;
					begin -- query_labels
						section_mark (section_junctions, HEADER);
						while junction_cursor /= type_junctions.no_element loop
							write (keyword => keyword_position, parameters => position (element (junction_cursor).coordinates));
							next (junction_cursor);
						end loop;
						section_mark (section_junctions, FOOTER);
					end query_junctions;

					procedure query_device_ports (segment : in type_net_segment) is
						use type_ports_component;
						port_cursor : type_ports_component.cursor := segment.component_ports.first;
					begin -- query_device_ports
						section_mark (section_ports, HEADER);
						while port_cursor /= type_ports_component.no_element loop
							write (keyword => keyword_device, parameters => 
								space & et_libraries.to_string (element (port_cursor).reference)
								& space & keyword_port & space
								& et_libraries.to_string (element (port_cursor).name)
								);
							next (port_cursor);
						end loop;
						section_mark (section_ports, FOOTER);
					end query_device_ports;

					procedure query_submodule_ports (segment : in type_net_segment) is
						use type_ports_submodule;
						port_cursor : type_ports_submodule.cursor := segment.submodule_ports.first;
					begin -- query_submodule_ports
						section_mark (section_submodule_ports, HEADER);
						while port_cursor /= type_ports_submodule.no_element loop
							section_mark (section_port, HEADER);

							-- module name
							write (keyword => keyword_module, parameters => 
								space & to_string (element (port_cursor).module));

							-- port name
							write (keyword => keyword_name, parameters => 
								space & et_libraries.to_string (element (port_cursor).port));

							-- port position
							write (keyword => keyword_position, parameters => position (element (port_cursor).position));

							-- port direction
							write (keyword => keyword_direction, parameters => et_libraries.to_string (element (port_cursor).direction));
							
							section_mark (section_port, FOOTER);
							next (port_cursor);
						end loop;
						section_mark (section_submodule_ports, FOOTER);
					end query_submodule_ports;

					
				begin -- query_strands
					section_mark (section_segments, HEADER);
					while segment_cursor /= type_net_segments.no_element loop
						section_mark (section_segment, HEADER);

						write (keyword => keyword_start, parameters => position (element (segment_cursor).coordinates_start));
						write (keyword => keyword_end,   parameters => position (element (segment_cursor).coordinates_end));

						query_element (segment_cursor, query_labels'access);
						query_element (segment_cursor, query_junctions'access);
						query_element (segment_cursor, query_device_ports'access);
						query_element (segment_cursor, query_submodule_ports'access);
						
						section_mark (section_segment, FOOTER);
						next (segment_cursor);
					end loop;
					section_mark (section_segments, FOOTER);
				end query_segments;
				
			begin -- query_segments
				section_mark (section_strands, HEADER);
				while strand_cursor /= type_strands.no_element loop
					section_mark (section_strand, HEADER);
					-- CS write strand position

					query_element (strand_cursor, query_segments'access);
					
					section_mark (section_strand, FOOTER);
					next (strand_cursor);
				end loop;
				
				section_mark (section_strands, FOOTER);
			end query_strands;

			procedure query_route (net_name : in type_net_name.bounded_string; net : in type_net) is
				use et_pcb;
				use type_copper_lines_pcb;
				line_cursor : type_copper_lines_pcb.cursor := net.route.lines.first;

				use type_copper_arcs_pcb;
				arc_cursor : type_copper_arcs_pcb.cursor := net.route.arcs.first;

				use type_vias;
				via_cursor : type_vias.cursor := net.route.vias.first;

				use type_copper_polygons_signal;
				polygon_cursor : type_copper_polygons_signal.cursor := net.route.polygons.first;

				procedure query_points (polygon : in type_copper_polygon_signal) is
					use type_polygon_points;
					point_cursor : type_polygon_points.cursor := polygon.points.first;
				begin
					section_mark (section_corners, HEADER);
					while point_cursor /= type_polygon_points.no_element loop
						write (keyword => keyword_position, parameters => position (element (point_cursor)));
						next (point_cursor);
					end loop;
					section_mark (section_corners, FOOTER);
				end query_points;
				
			begin -- query_route
				section_mark (section_route, HEADER);

				while line_cursor /= type_copper_lines_pcb.no_element loop
					section_mark (section_line, HEADER);
					
					write (keyword => keyword_start, parameters => position (element (line_cursor).start_point));
					write (keyword => keyword_end  , parameters => position (element (line_cursor).end_point));
					write (keyword => keyword_layer, parameters => to_string (element (line_cursor).layer));
					write (keyword => keyword_width, parameters => et_pcb_coordinates.to_string (element (line_cursor).width));

					section_mark (section_line, FOOTER);
					next (line_cursor);
				end loop;

				while arc_cursor /= type_copper_arcs_pcb.no_element loop
					section_mark (section_arc, HEADER);

					write (keyword => keyword_center, parameters => position (element (arc_cursor).center));
					write (keyword => keyword_start , parameters => position (element (arc_cursor).start_point));
					write (keyword => keyword_end   , parameters => position (element (arc_cursor).end_point));
					write (keyword => keyword_width , parameters => et_pcb_coordinates.to_string (element (arc_cursor).width));
					write (keyword => keyword_layer , parameters => to_string (element (arc_cursor).layer));
					
					section_mark (section_arc, FOOTER);
					next (arc_cursor);
				end loop;

				while via_cursor /= type_vias.no_element loop
					section_mark (section_via, HEADER);

					write (keyword => keyword_position, parameters => position (element (via_cursor).position));
					write (keyword => keyword_diameter, parameters => et_pcb_coordinates.to_string (element (via_cursor).diameter));
					write (keyword => keyword_layer_start, parameters => to_string (element (via_cursor).layer_start));
					write (keyword => keyword_layer_end  , parameters => to_string (element (via_cursor).layer_end));
					write (keyword => keyword_restring_outer_layers, parameters => et_pcb_coordinates.to_string (element (via_cursor).restring_outer));
					write (keyword => keyword_restring_inner_layers, parameters => et_pcb_coordinates.to_string (element (via_cursor).restring_inner));
					
					section_mark (section_via, FOOTER);
					next (via_cursor);
				end loop;
				
				while polygon_cursor /= type_copper_polygons_signal.no_element loop
					section_mark (section_polygon, HEADER);

					write (keyword => keyword_priority , parameters => et_pcb.to_string (element (polygon_cursor).priority_level));
					write (keyword => keyword_layer , parameters => to_string (element (polygon_cursor).layer));
					write (keyword => keyword_min_width , parameters => et_pcb_coordinates.to_string (element (polygon_cursor).width_min));
					write (keyword => keyword_isolation, parameters => et_pcb_coordinates.to_string (element (polygon_cursor).isolation_gap));
					write (keyword => keyword_fill_style, parameters => to_string (element (polygon_cursor).fill_style));
					write (keyword => keyword_hatching_line_width  , parameters => et_pcb_coordinates.to_string (element (polygon_cursor).hatching_line_width));
					write (keyword => keyword_hatching_line_spacing, parameters => et_pcb_coordinates.to_string (element (polygon_cursor).hatching_spacing));
					write (keyword => keyword_corner_easing, parameters => to_string (element (polygon_cursor).corner_easing));
					write (keyword => keyword_easing_radius, parameters => et_pcb_coordinates.to_string (element (polygon_cursor).easing_radius));

					case element (polygon_cursor).pad_connection is
						when THERMAL => 
							write (keyword => keyword_pad_connection, parameters => et_pcb.to_string (element (polygon_cursor).pad_connection));
							write (keyword => keyword_pad_technology, parameters => et_pcb.to_string (element (polygon_cursor).thermal_technology));
							write (keyword => keyword_thermal_width , parameters => et_pcb_coordinates.to_string (element (polygon_cursor).thermal_width));
							write (keyword => keyword_thermal_gap   , parameters => et_pcb_coordinates.to_string (element (polygon_cursor).thermal_gap));	
							
						when SOLID =>
							write (keyword => keyword_pad_technology, parameters => et_pcb.to_string (element (polygon_cursor).solid_technology));	
							
						when NONE => null;
					end case;

					query_element (polygon_cursor, query_points'access);
					section_mark (section_polygon, FOOTER);
					next (polygon_cursor);
				end loop;
				
				section_mark (section_route, FOOTER);
			end query_route;
			
		begin -- query_nets
			log_indentation_up;
			section_mark (section_nets, HEADER);
			while net_cursor /= type_nets.no_element loop
				log ("net " & to_string (key (net_cursor)), log_threshold + 1);
				section_mark (section_net, HEADER);

				write (keyword => keyword_name, parameters => to_string (key (net_cursor)), space => true);
				write (keyword => keyword_class, parameters => to_string (element (net_cursor).class), space => true);
				write (keyword => keyword_scope, parameters => to_string (element (net_cursor).scope));

				query_element (net_cursor, query_strands'access);
				query_element (net_cursor, query_route'access);
				
				section_mark (section_net, FOOTER);
				next (net_cursor);
			end loop;
			section_mark (section_nets, FOOTER);
			
			log_indentation_down;
		end query_nets;

		procedure query_devices is		
			use et_schematic;
			use type_devices;
			device_cursor : et_schematic.type_devices.cursor := et_schematic.module.devices.first;

			procedure query_units (device_name : in et_libraries.type_component_reference; device : in et_schematic.type_device) is
				use et_schematic.type_units;
				unit_cursor : type_units.cursor := device.units.first;

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
					write (keyword => keyword_rotation, parameters => rotation (element (unit_cursor).orientation)); -- rotation 180.0
					write (keyword => keyword_mirrored, parameters => to_string (element (unit_cursor).mirror, verbose => false)); -- x_axis, y_axis, none

					section_mark (section_placeholders, HEADER);

					write_placeholder (element (unit_cursor).reference);
					write_placeholder (element (unit_cursor).value);

					if element (unit_cursor).appearance = et_libraries.SCH_PCB then
						write_placeholder (element (unit_cursor).purpose);
						write_placeholder (element (unit_cursor).partcode);
						write_placeholder (element (unit_cursor).bom);
					end if;

					section_mark (section_placeholders, FOOTER);
					
					section_mark (section_unit, FOOTER);
					next (unit_cursor);
				end loop;
				section_mark (section_units, FOOTER);
			end query_units;

			procedure query_placeholders (device_name : in et_libraries.type_component_reference; device : in et_schematic.type_device) is
				use et_pcb;
				use et_pcb.type_text_placeholders_package;
				placeholder_cursor : type_text_placeholders_package.cursor;

				procedure write_placeholder (placeholder_cursor : in type_text_placeholders_package.cursor) is 
				begin
					section_mark (section_placeholder, HEADER);
					write (keyword => keyword_meaning, parameters => to_string (element (placeholder_cursor).meaning));
					write_text_properties (element (placeholder_cursor));
					section_mark (section_placeholder, FOOTER);					
				end write_placeholder;
				
			begin -- query_placeholders
				section_mark (section_placeholders, HEADER);
				
				device.text_placeholders.silk_screen.top.iterate (write_placeholder'access);
				device.text_placeholders.silk_screen.bottom.iterate (write_placeholder'access);
				device.text_placeholders.assy_doc.top.iterate (write_placeholder'access);
				device.text_placeholders.assy_doc.bottom.iterate (write_placeholder'access);
				
				section_mark (section_placeholders, FOOTER);				
			end query_placeholders;
			
		begin -- query_devices
			section_mark (section_devices, HEADER);			
			while device_cursor /= type_devices.no_element loop
				section_mark (section_device, HEADER);
				write (keyword => keyword_name, parameters => et_libraries.to_string (key (device_cursor)), space => true);
				write (keyword => keyword_appearance, parameters => et_libraries.to_string (element (device_cursor).appearance));
				write (keyword => keyword_value, parameters => et_libraries.to_string (element (device_cursor).value), space => true);
				write (keyword => keyword_model, parameters => et_libraries.to_string (element (device_cursor).model), space => true);

				case element (device_cursor).appearance is
					when et_libraries.SCH_PCB =>
						write (keyword => keyword_variant , parameters => et_libraries.to_string (element (device_cursor).variant), space => true);
						write (keyword => keyword_partcode, parameters => et_libraries.to_string (element (device_cursor).partcode), space => true);
						write (keyword => keyword_purpose , parameters => et_libraries.to_string (element (device_cursor).purpose), space => true, wrap => true);
						write (keyword => keyword_bom     , parameters => et_libraries.to_string (element (device_cursor).bom));
						
						section_mark (section_package, HEADER);
						write (keyword => keyword_position, parameters => position (element (device_cursor).position)); -- position in board !
						write (keyword => keyword_rotation, parameters => rotation (element (device_cursor).position)); -- rotation in board !
						write (keyword => keyword_face    , parameters => face (element (device_cursor).position));

						query_element (device_cursor, query_placeholders'access);
						section_mark (section_package, FOOTER);
					when et_libraries.SCH => null;
				end case;

				query_element (device_cursor, query_units'access);
				
				section_mark (section_device, FOOTER);
				next (device_cursor);
			end loop;
			section_mark (section_devices, FOOTER);
		end query_devices;

		procedure query_frames is		
			-- CS: handle sheet description 
			use et_libraries;
			use type_frame_template_name;
		begin
			section_mark (section_drawing_frames, HEADER);
			section_mark (section_schematic, HEADER);			
			write (keyword => keyword_template, parameters => to_string (et_schematic.module.frame_template_schematic));
			section_mark (section_schematic, FOOTER);			

			section_mark (section_board, HEADER);			
			write (keyword => keyword_template, parameters => to_string (et_schematic.module.frame_template_board));
			section_mark (section_board, FOOTER);			
			section_mark (section_drawing_frames, FOOTER);
		end query_frames;

		procedure query_submodules is		
			use et_schematic;
			use type_submodules;
			submodule_cursor : type_submodules.cursor := et_schematic.module.submodules.first;
		begin
			section_mark (section_submodules, HEADER);
			while submodule_cursor /= type_submodules.no_element loop
				section_mark (section_submodule, HEADER);
				write (keyword => keyword_name, space => true, parameters => type_submodule_path.to_string (key (submodule_cursor)));
				write (keyword => keyword_path, space => true, parameters => type_submodule_name.to_string (element (submodule_cursor).name));
				write (keyword => keyword_position, parameters => position (element (submodule_cursor).position));
				write (keyword => keyword_size, parameters => 
					space & keyword_pos_x & to_string (element (submodule_cursor).size.x) &
					space & keyword_pos_y & to_string (element (submodule_cursor).size.y)); -- size x 50 y 70
				write (keyword => keyword_position_in_board, parameters => position (element (submodule_cursor).position_in_board));
				write (keyword => keyword_rotation, parameters => rotation (element (submodule_cursor).position_in_board));
				write (keyword => keyword_view_mode, parameters => to_string (element (submodule_cursor).view_mode));
				write (keyword => keyword_reference_offset, parameters => et_libraries.to_string (element (submodule_cursor).reference_offset));
				section_mark (section_submodule, FOOTER);				
				next (submodule_cursor);
			end loop;
			section_mark (section_submodules, FOOTER);
		end query_submodules;

		procedure query_texts is		
			use et_schematic;
			use type_texts;
			text_cursor : type_texts.cursor := et_schematic.module.texts.first;
		begin
			section_mark (section_texts, HEADER);
			while text_cursor /= type_texts.no_element loop
				section_mark (section_text, HEADER);
				write (keyword => keyword_position, parameters => position (element (text_cursor).coordinates));
				write (keyword => keyword_content, space => true, wrap => true,
					   parameters => et_libraries.to_string (element (text_cursor).content));
				write_text_properties (element (text_cursor));
				section_mark (section_text, FOOTER);
				next (text_cursor);
			end loop;
			section_mark (section_texts, FOOTER);
		end query_texts;

		procedure query_board is
			use et_pcb;
			use et_pcb_coordinates;

			use type_texts_with_content;
			use type_text_placeholders_pcb;

			use type_silk_lines;
			use type_silk_arcs;
			use type_silk_circles;
			use type_silk_polygons;

			use type_doc_lines;
			use type_doc_arcs;
			use type_doc_circles;
			use type_doc_polygons;

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

			use type_copper_lines_pcb;
			use type_copper_arcs_pcb;
			use type_copper_circles_pcb;
			use type_copper_polygons_floating;
			use type_texts_with_content_pcb;
			use type_text_placeholders_copper;

			use type_pcb_contour_lines;
			use type_pcb_contour_arcs;
			use type_pcb_contour_circles;
			
			-- general stuff
			procedure write_placeholder (cursor : in type_text_placeholders_pcb.cursor) is
			begin
				placeholder_begin;
				write (keyword => keyword_meaning, parameters => to_string (element (cursor).meaning));
				write_text_properties (element (cursor));
				placeholder_end;
			end write_placeholder;

			-- COPPER (NON-ELECTRIC)
			procedure write_line (cursor : in type_copper_lines_pcb.cursor) is begin
				line_begin;
				write (keyword => keyword_start, parameters => position (element (cursor).start_point));
				write (keyword => keyword_end  , parameters => position (element (cursor).end_point));
				write (keyword => keyword_width, parameters => to_string (element (cursor).width));
				write (keyword => keyword_layer, parameters => to_string (element (cursor).layer));
				line_end;
			end write_line;

			procedure write_arc (cursor : in type_copper_arcs_pcb.cursor) is begin
				arc_begin;
				write (keyword => keyword_center, parameters => position (element (cursor).center));
				write (keyword => keyword_start, parameters => position (element (cursor).start_point));
				write (keyword => keyword_end  , parameters => position (element (cursor).end_point));
				write (keyword => keyword_width, parameters => to_string (element (cursor).width));
				write (keyword => keyword_layer, parameters => to_string (element (cursor).layer));
				arc_end;
			end write_arc;

			procedure write_circle (cursor : in type_copper_circles_pcb.cursor) is begin
				circle_begin;
				write (keyword => keyword_center, parameters => position (element (cursor).center));
				write (keyword => keyword_radius, parameters => to_string (element (cursor).radius));
				write (keyword => keyword_width , parameters => to_string (element (cursor).width));
				write (keyword => keyword_layer, parameters => to_string (element (cursor).layer));
				circle_end;
			end write_circle;
			
			procedure write_polygon (cursor : in type_copper_polygons_floating.cursor) is 
				use type_polygon_points;
				
				procedure query_points (polygon : in type_copper_polygon_floating) is begin
					iterate (polygon.points, write_polygon_corners'access); -- see general stuff above
				end query_points;
				
			begin -- write_polygon
				polygon_begin;
				write (keyword => keyword_fill_style, parameters => to_string (element (cursor).fill_style));
				write (keyword => keyword_hatching_line_width  , parameters => to_string (element (cursor).hatching_line_width));
				write (keyword => keyword_hatching_line_spacing, parameters => to_string (element (cursor).hatching_spacing));
				write (keyword => keyword_corner_easing, parameters => to_string (element (cursor).corner_easing));
				write (keyword => keyword_easing_radius, parameters => to_string (element (cursor).easing_radius));
				write (keyword => keyword_layer, parameters => to_string (element (cursor).layer));
				corners_begin;
				query_element (cursor, query_points'access);
				corners_end;
				polygon_end;
			end write_polygon;

			procedure write_text (cursor : in type_texts_with_content_pcb.cursor) is -- copper texts in board !
			begin
				text_begin;
				write (keyword => keyword_content, parameters => et_libraries.to_string (element (cursor).content));
				write_text_properties (element (cursor));
				write (keyword => keyword_layer, parameters => to_string (element (cursor).layer));
				text_end;
			end write_text;

			procedure write_placeholder (cursor : in type_text_placeholders_copper.cursor) is -- placeholders in board copper !
			begin
				placeholder_begin;
				write (keyword => keyword_meaning, parameters => to_string (element (cursor).meaning));
				write_text_properties (element (cursor));
				write (keyword => keyword_layer, parameters => to_string (element (cursor).layer));
				placeholder_end;
			end write_placeholder;
			
			
		begin -- query_board
			section_mark (section_board, HEADER);

			-- SILK SCREEN
			section_mark (section_silk_screen, HEADER);

				section_mark (section_top, HEADER);
				iterate (et_schematic.module.board.silk_screen.top.lines, write_line'access);
				iterate (et_schematic.module.board.silk_screen.top.arcs, write_arc'access);
				iterate (et_schematic.module.board.silk_screen.top.circles, write_circle'access);
				iterate (et_schematic.module.board.silk_screen.top.polygons, write_polygon'access);
				iterate (et_schematic.module.board.silk_screen.top.texts, write_text'access);
				iterate (et_schematic.module.board.silk_screen.top.placeholders, write_placeholder'access);
				section_mark (section_top, FOOTER);

				section_mark (section_bottom, HEADER);
				iterate (et_schematic.module.board.silk_screen.bottom.lines, write_line'access);
				iterate (et_schematic.module.board.silk_screen.bottom.arcs, write_arc'access);
				iterate (et_schematic.module.board.silk_screen.bottom.circles, write_circle'access);
				iterate (et_schematic.module.board.silk_screen.bottom.polygons, write_polygon'access);
				iterate (et_schematic.module.board.silk_screen.bottom.texts, write_text'access);
				iterate (et_schematic.module.board.silk_screen.bottom.placeholders, write_placeholder'access);
				section_mark (section_bottom, FOOTER);
			
			section_mark (section_silk_screen, FOOTER);

			-- ASSEMBLY DOCUMENTATION
			section_mark (section_assembly_doc, HEADER);

			section_mark (section_top, HEADER);
				iterate (et_schematic.module.board.assy_doc.top.lines, write_line'access);
				iterate (et_schematic.module.board.assy_doc.top.arcs, write_arc'access);
				iterate (et_schematic.module.board.assy_doc.top.circles, write_circle'access);
				iterate (et_schematic.module.board.assy_doc.top.polygons, write_polygon'access);
				iterate (et_schematic.module.board.assy_doc.top.texts, write_text'access);
				iterate (et_schematic.module.board.assy_doc.top.placeholders, write_placeholder'access);
				section_mark (section_top, FOOTER);

				section_mark (section_bottom, HEADER);
				iterate (et_schematic.module.board.assy_doc.bottom.lines, write_line'access);
				iterate (et_schematic.module.board.assy_doc.bottom.arcs, write_arc'access);
				iterate (et_schematic.module.board.assy_doc.bottom.circles, write_circle'access);
				iterate (et_schematic.module.board.assy_doc.bottom.polygons, write_polygon'access);
				iterate (et_schematic.module.board.assy_doc.bottom.texts, write_text'access);
				iterate (et_schematic.module.board.assy_doc.bottom.placeholders, write_placeholder'access);
				section_mark (section_bottom, FOOTER);

			section_mark (section_assembly_doc, FOOTER);

			-- STENCIL
			section_mark (section_stencil, HEADER);

			section_mark (section_top, HEADER);
				iterate (et_schematic.module.board.stencil.top.lines, write_line'access);
				iterate (et_schematic.module.board.stencil.top.arcs, write_arc'access);
				iterate (et_schematic.module.board.stencil.top.circles, write_circle'access);
				iterate (et_schematic.module.board.stencil.top.polygons, write_polygon'access);
				section_mark (section_top, FOOTER);

				section_mark (section_bottom, HEADER);
				iterate (et_schematic.module.board.stencil.bottom.lines, write_line'access);
				iterate (et_schematic.module.board.stencil.bottom.arcs, write_arc'access);
				iterate (et_schematic.module.board.stencil.bottom.circles, write_circle'access);
				iterate (et_schematic.module.board.stencil.bottom.polygons, write_polygon'access);
				section_mark (section_bottom, FOOTER);

			section_mark (section_stencil, FOOTER);

			-- STOP MASK
			section_mark (section_stop_mask, HEADER);

			section_mark (section_top, HEADER);
				iterate (et_schematic.module.board.stop_mask.top.lines, write_line'access);
				iterate (et_schematic.module.board.stop_mask.top.arcs, write_arc'access);
				iterate (et_schematic.module.board.stop_mask.top.circles, write_circle'access);
				iterate (et_schematic.module.board.stop_mask.top.polygons, write_polygon'access);
				iterate (et_schematic.module.board.stop_mask.top.texts, write_text'access);			
				section_mark (section_top, FOOTER);

				section_mark (section_bottom, HEADER);
				iterate (et_schematic.module.board.stop_mask.bottom.lines, write_line'access);
				iterate (et_schematic.module.board.stop_mask.bottom.arcs, write_arc'access);
				iterate (et_schematic.module.board.stop_mask.bottom.circles, write_circle'access);
				iterate (et_schematic.module.board.stop_mask.bottom.polygons, write_polygon'access);
				iterate (et_schematic.module.board.stop_mask.bottom.texts, write_text'access);
				section_mark (section_bottom, FOOTER);

			section_mark (section_stop_mask, FOOTER);

			-- KEEPOUT
			section_mark (section_keepout, HEADER);

			section_mark (section_top, HEADER);
				iterate (et_schematic.module.board.keepout.top.lines, write_line'access);
				iterate (et_schematic.module.board.keepout.top.arcs, write_arc'access);
				iterate (et_schematic.module.board.keepout.top.circles, write_circle'access);
				iterate (et_schematic.module.board.keepout.top.polygons, write_polygon'access);
				-- CS iterate (et_schematic.module.board.keepout.top.texts, write_text'access);
				section_mark (section_top, FOOTER);

				section_mark (section_bottom, HEADER);
				iterate (et_schematic.module.board.keepout.bottom.lines, write_line'access);
				iterate (et_schematic.module.board.keepout.bottom.arcs, write_arc'access);
				iterate (et_schematic.module.board.keepout.bottom.circles, write_circle'access);
				iterate (et_schematic.module.board.keepout.bottom.polygons, write_polygon'access);
				-- CS iterate (et_schematic.module.board.keepout.bottom.texts, write_text'access);
				section_mark (section_bottom, FOOTER);

			section_mark (section_keepout, FOOTER);

			-- ROUTE RESTRICT
			section_mark (section_route_restrict, HEADER);
				iterate (et_schematic.module.board.route_restrict.lines, write_line'access);
				iterate (et_schematic.module.board.route_restrict.arcs, write_arc'access);
				iterate (et_schematic.module.board.route_restrict.circles, write_circle'access);
				iterate (et_schematic.module.board.route_restrict.polygons, write_polygon'access);
				-- CS iterate (et_schematic.module.board.route_restrict.texts, write_text'access);
			section_mark (section_route_restrict, FOOTER);

			-- VIA RESTRICT
			section_mark (section_via_restrict, HEADER);
				iterate (et_schematic.module.board.via_restrict.lines, write_line'access);
				iterate (et_schematic.module.board.via_restrict.arcs, write_arc'access);
				iterate (et_schematic.module.board.via_restrict.circles, write_circle'access);
				iterate (et_schematic.module.board.via_restrict.polygons, write_polygon'access);
				-- CS iterate (et_schematic.module.board.via_restrict.texts, write_text'access);
			section_mark (section_via_restrict, FOOTER);

			-- COPPER (NON-ELECTRIC)
			section_mark (section_copper, HEADER);
				iterate (et_schematic.module.board.copper.lines, write_line'access);
				iterate (et_schematic.module.board.copper.arcs, write_arc'access);
				iterate (et_schematic.module.board.copper.circles, write_circle'access);
				iterate (et_schematic.module.board.copper.polygons, write_polygon'access);
				iterate (et_schematic.module.board.copper.texts, write_text'access);
				iterate (et_schematic.module.board.copper.placeholders, write_placeholder'access);
			section_mark (section_copper, FOOTER);

			-- BOARD CONTOUR
			section_mark (section_pcb_contour, HEADER);
				iterate (et_schematic.module.board.contour.lines, write_line'access);
				iterate (et_schematic.module.board.contour.arcs, write_arc'access);
				iterate (et_schematic.module.board.contour.circles, write_circle'access);
			section_mark (section_pcb_contour, FOOTER);
			
			---BOARD END-----
			section_mark (section_board, FOOTER);
		end query_board;

	
	begin -- save_module
		log ("saving module ...", log_threshold);

		reset_tab_depth;
		
		log_indentation_up;

		
		write_module_header;

		
		-- net classes
		query_net_classes;

		-- nets
		query_nets;

		-- frames
		query_frames;
		
		-- notes
		query_texts;
		
		-- submodules
		query_submodules;
		
		-- devices
		query_devices;
		
		-- board
		query_board;
	
	
		write_module_footer;

		
		log_indentation_down;

		exception when event:
			others => 
				log (ada.exceptions.exception_message (event), console => true);
				close (module_file_handle);
				raise;

	end save_module;


	procedure write_symbol (
		symbol			: in et_libraries.type_symbol;
		log_threshold	: in et_string_processing.type_log_level) is
		use et_libraries;
		use type_lines;
		use type_polylines;
		use type_rectangles;
		use type_arcs;
		use type_circles;
		use type_symbol_texts;
		use type_ports;

		procedure write_line (cursor : in type_lines.cursor) is begin
			section_mark (section_line, HEADER);
			write (keyword => keyword_start, parameters => position (element (cursor).start_point));
			write (keyword => keyword_end  , parameters => position (element (cursor).end_point));
			write (keyword => keyword_width, parameters => et_coordinates.to_string (element (cursor).line_width));
			section_mark (section_line, FOOTER);
		end write_line;

		procedure write_arc (cursor : in type_arcs.cursor) is begin
			section_mark (section_arc, HEADER);
			write (keyword => keyword_center, parameters => position (element (cursor).center));
			write (keyword => keyword_start , parameters => position (element (cursor).start_point));
			write (keyword => keyword_end   , parameters => position (element (cursor).end_point));
			write (keyword => keyword_radius, parameters => et_coordinates.to_string (element (cursor).radius));			
			write (keyword => keyword_width , parameters => et_coordinates.to_string (element (cursor).line_width));
			write (keyword => keyword_fill  , parameters => to_string (element (cursor).fill));
			section_mark (section_arc, FOOTER);
		end write_arc;

		procedure write_circle (cursor : in type_circles.cursor) is begin
			section_mark (section_circle, HEADER);
			write (keyword => keyword_center, parameters => position (element (cursor).center));
			write (keyword => keyword_radius, parameters => et_coordinates.to_string (element (cursor).radius));
			write (keyword => keyword_width , parameters => et_coordinates.to_string (element (cursor).line_width));
			write (keyword => keyword_fill  , parameters => to_string (element (cursor).fill));
			section_mark (section_arc, FOOTER);
		end write_circle;

		procedure write_rectangle (cursor : in type_rectangles.cursor) is begin
			section_mark (section_rectangle, HEADER);
			write (keyword => keyword_corner_a, parameters => position (element (cursor).start_point));
			write (keyword => keyword_corner_b, parameters => position (element (cursor).end_point));
			write (keyword => keyword_width , parameters => et_coordinates.to_string (element (cursor).line_width));
			write (keyword => keyword_fill  , parameters => to_string (element (cursor).fill));
			section_mark (section_rectangle, FOOTER);
		end write_rectangle;
		
		procedure write_polyline (cursor : in type_polylines.cursor) is 
			line : type_polyline := element (cursor);
			use type_points;

			procedure write_point (cursor : in type_points.cursor) is begin
				write (keyword_position, parameters => position (element (cursor)));
			end write_point;
		
		begin -- write_polyline
			section_mark (section_polyline, HEADER);
			write (keyword => keyword_width , parameters => et_coordinates.to_string (line.line_width));
			write (keyword => keyword_fill  , parameters => to_string (line.fill));
			section_mark (section_corners, HEADER);
			iterate (line.points, write_point'access);
			section_mark (section_corners, FOOTER);
			section_mark (section_polyline, FOOTER);
		end write_polyline;

		procedure write_text (cursor : in type_symbol_texts.cursor) is begin
			section_mark (section_text, HEADER);
			write (keyword => keyword_position, parameters => position (element (cursor).position));
			write (keyword => keyword_content , parameters => to_string (element (cursor).content));			
			write_text_properties (element (cursor));
			section_mark (section_text, FOOTER);
		end write_text;

		procedure write_placeholders is
		begin
			section_mark (section_placeholders, HEADER);

			section_mark (section_placeholder, HEADER);
			write (keyword => keyword_meaning , parameters => to_string (symbol.reference.meaning));
			write (keyword => keyword_position, parameters => position (symbol.reference.position));
			write_text_properties (symbol.reference);
			section_mark (section_placeholder, FOOTER);

			section_mark (section_placeholder, HEADER);
			write (keyword => keyword_meaning , parameters => to_string (symbol.value.meaning));
			write (keyword => keyword_position, parameters => position (symbol.value.position));
			write_text_properties (symbol.value);
			section_mark (section_placeholder, FOOTER);

			case symbol.appearance is
				when et_libraries.SCH_PCB =>
					section_mark (section_placeholder, HEADER);
					write (keyword => keyword_meaning , parameters => to_string (symbol.purpose.meaning));
					write (keyword => keyword_position, parameters => position (symbol.purpose.position));
					write_text_properties (symbol.purpose);
					section_mark (section_placeholder, FOOTER);
				when others => null;
			end case;
			
			section_mark (section_placeholders, FOOTER);
		end write_placeholders;

		procedure write_port (cursor : in type_ports.cursor) is begin
			section_mark (section_port, HEADER);
			write (keyword => keyword_name, space => true, parameters => to_string (element (cursor).name));
			write (keyword => keyword_position, parameters => position (element (cursor).coordinates));
			write (keyword => keyword_direction, parameters => to_string (element (cursor).direction, preamble => false));
			write (keyword => keyword_characteristic, parameters => to_string (element (cursor).characteristic));
			write (keyword => keyword_length, parameters => et_coordinates.to_string (element (cursor).length));
			write (keyword => keyword_rotation, parameters => rotation (element (cursor).orientation));
			write (keyword => keyword_port_name_visible, parameters => to_string (element (cursor).port_name_visible));
			write (keyword => keyword_port_name_size, parameters => et_coordinates.to_string (element (cursor).port_name_size));
			write (keyword => keyword_terminal_name_visible, parameters => to_string (element (cursor).terminal_visible));
			write (keyword => keyword_terminal_name_size, parameters => et_coordinates.to_string (element (cursor).terminal_name_size));
			section_mark (section_port, FOOTER);			
		end write_port;
		
	begin -- write_symbol
		
		-- SHAPES BEGIN
		section_mark (section_shapes, HEADER);
		
			-- lines
			iterate (symbol.shapes.lines, write_line'access);

			-- arcs
			iterate (symbol.shapes.arcs, write_arc'access);
			
			-- circles
			iterate (symbol.shapes.circles, write_circle'access);

			-- rectangles
			iterate (symbol.shapes.rectangles, write_rectangle'access);
			
			-- polylines
			iterate (symbol.shapes.polylines, write_polyline'access);

		section_mark (section_shapes, FOOTER);
		-- SHAPES END
		
		-- TEXTS
		section_mark (section_texts, HEADER);
		iterate (symbol.texts, write_text'access);
		section_mark (section_texts, FOOTER);

		-- PLACEHOLDERS
		section_mark (section_placeholders, HEADER);
		write_placeholders;
		section_mark (section_placeholders, FOOTER);

		-- PORTS
		section_mark (section_ports, HEADER);
		iterate (symbol.ports, write_port'access);
		section_mark (section_ports, FOOTER);
	end write_symbol;

	
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
					to_string (key (terminal_cursor)) & space 		-- terminal name like G14 or 16
					& keyword_unit & space & to_string (element (terminal_cursor).unit) -- unit name like A,B or GPIO_BANK_1
					& space & to_string (element (terminal_cursor).name) 	-- port name like CE, WE, GND
					);
			end write_terminal;

		begin -- write_variant
			write (keyword => keyword_file, space => true, parameters => to_string (variant.packge)); -- CS path correct ??
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
			write (keyword => keyword_position, parameters => position (unit.coordinates));
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
			write (keyword => keyword_position, parameters => position (unit.coordinates));
			write (keyword => keyword_swap_level, parameters => to_string (unit.swap_level));
			write (keyword => keyword_add_level , parameters => to_string (unit.add_level));
			write (keyword => keyword_file, space => true, parameters => to_string (unit.file));
		end query_external_unit;
		
	begin -- save_device
		
		log (name, log_threshold);

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

		-- prefix, value, ...
		write (keyword => keyword_prefix, space => true, parameters => to_string (device.prefix));
		write (keyword => keyword_value , space => true, parameters => to_string (device.value));
		write (keyword => keyword_commissioned, parameters => to_string (device.commissioned));
		write (keyword => keyword_updated     , parameters => to_string (device.updated));
		write (keyword => keyword_author      , parameters => to_string (device.author));

		-- package variants
		case device.appearance is
			when SCH_PCB =>
				write (keyword => keyword_purpose , space => true, parameters => to_string (device.purpose));
				write (keyword => keyword_partcode, space => true, parameters => to_string (device.partcode));
				write (keyword => keyword_bom     , parameters => to_string (device.bom));

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
			log (ada.exceptions.exception_message (event));
			if is_open (file_handle) then
				close (file_handle);
			end if;
			raise;
		
	end save_device;

	procedure save_symbol ( -- CS: testing requried
	-- Saves the given symbol model in a file specified by name.
		name			: in string; -- libraries/symbols/resistor.sym
		symbol			: in et_libraries.type_symbol; -- the actual symbol model
		log_threshold	: in et_string_processing.type_log_level) is
		use et_string_processing;
		file_handle : ada.text_io.file_type;
	begin
		log (name, log_threshold);

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
			log (ada.exceptions.exception_message (event));
			if is_open (file_handle) then
				close (file_handle);
			end if;
			raise;
		
	end save_symbol;
	
	procedure save_package (
	-- Saves the given package model in a file specified by name.
		name			: in string; -- libraries/packages/resistor.pac
		packge			: in et_pcb.type_package; -- the actual package model
		log_threshold	: in et_string_processing.type_log_level) is
		use et_string_processing;
		use et_pcb;
		
		file_handle : ada.text_io.file_type;

		use type_texts_with_content;
		use type_text_placeholders_package;
		
		use type_copper_lines;
		use type_copper_arcs;
		use type_copper_circles;
		use type_texts_with_content;
		use type_copper_polygons;

		use type_keepout_lines;
		use type_keepout_arcs;
		use type_keepout_circles;
		use type_keepout_polygons;

		use type_stop_lines;
		use type_stop_arcs;
		use type_stop_circles;
		use type_stop_polygons;

		use type_stencil_lines;
		use type_stencil_arcs;
		use type_stencil_circles;
		use type_stencil_polygons;

		use type_route_restrict_lines;
		use type_route_restrict_arcs;
		use type_route_restrict_circles;
		use type_route_restrict_polygons;

		use type_via_restrict_lines;
		use type_via_restrict_arcs;
		use type_via_restrict_circles;
		use type_via_restrict_polygons;

		use type_pcb_contour_lines;
		use type_pcb_contour_arcs;
		use type_pcb_contour_circles;

		use type_silk_lines;
		use type_silk_arcs;
		use type_silk_circles;
		use type_silk_polygons;

		use type_doc_lines;
		use type_doc_arcs;
		use type_doc_circles;
		use type_doc_polygons;
				
		procedure write_copper is

			procedure write_line (cursor : in type_copper_lines.cursor) is begin
				line_begin;
				write (keyword => keyword_start, parameters => position (element (cursor).start_point));
				write (keyword => keyword_end  , parameters => position (element (cursor).end_point));
				write (keyword => keyword_width, parameters => et_pcb_coordinates.to_string (element (cursor).width));
				line_end;
			end write_line;

			procedure write_arc (cursor : in type_copper_arcs.cursor) is begin
				arc_begin;
				write (keyword => keyword_center, parameters => position (element (cursor).center));				
				write (keyword => keyword_start , parameters => position (element (cursor).start_point));
				write (keyword => keyword_end   , parameters => position (element (cursor).end_point));
				write (keyword => keyword_width , parameters => et_pcb_coordinates.to_string (element (cursor).width));
				arc_end;
			end write_arc;

			procedure write_circle (cursor : in type_copper_circles.cursor) is begin
				circle_begin;
				write (keyword => keyword_center, parameters => position (element (cursor).center));				
				write (keyword => keyword_radius, parameters => et_pcb_coordinates.to_string (element (cursor).radius));
				write (keyword => keyword_width , parameters => et_pcb_coordinates.to_string (element (cursor).width));
				circle_end;
			end write_circle;

			procedure write_polygon (cursor : in type_copper_polygons.cursor) is 
				use et_pcb_coordinates;
				use type_polygon_points;
				
				procedure query_points (polygon : in type_copper_polygon) is begin
					iterate (polygon.points, write_polygon_corners'access);
				end query_points;

			begin -- write_polygon
				polygon_begin;
				write (keyword => keyword_priority, parameters => et_pcb.to_string (element (cursor).priority_level));
				write (keyword => keyword_isolation, parameters => to_string (element (cursor).isolation_gap));
				write (keyword => keyword_fill_style, parameters => to_string (element (cursor).fill_style));
				write (keyword => keyword_hatching_line_width  , parameters => et_pcb_coordinates.to_string (element (cursor).hatching_line_width));
				write (keyword => keyword_hatching_line_spacing, parameters => et_pcb_coordinates.to_string (element (cursor).hatching_spacing));
				write (keyword => keyword_corner_easing, parameters => to_string (element (cursor).corner_easing));
				write (keyword => keyword_easing_radius, parameters => et_pcb_coordinates.to_string (element (cursor).easing_radius));
				corners_begin;
				query_element (cursor, query_points'access);
				corners_end;
				polygon_end;
			end write_polygon;

			
		begin -- write_copper
			section_mark (section_copper, HEADER);

			-- top
			section_mark (section_top, HEADER);			
			iterate (packge.copper.top.lines, write_line'access);
			iterate (packge.copper.top.arcs, write_arc'access);
			iterate (packge.copper.top.circles, write_circle'access);
			iterate (packge.copper.top.texts, write_text'access);
			section_mark (section_top, FOOTER);

			-- bottom
			section_mark (section_bottom, HEADER);			
			iterate (packge.copper.bottom.lines, write_line'access);
			iterate (packge.copper.bottom.arcs, write_arc'access);
			iterate (packge.copper.bottom.circles, write_circle'access);
			iterate (packge.copper.bottom.texts, write_text'access);			
			section_mark (section_bottom, FOOTER);

			section_mark (section_copper, FOOTER);
		end write_copper;

		procedure write_placeholder (cursor : in type_text_placeholders_package.cursor) is
		begin
			placeholder_begin;
			write (keyword => keyword_meaning, parameters => to_string (element (cursor).meaning));
			write_text_properties (element (cursor));
			placeholder_end;
		end write_placeholder;
		
		procedure write_silk_screen is begin
			section_mark (section_silk_screen, HEADER);

			-- top
			section_mark (section_top, HEADER);
			iterate (packge.silk_screen.top.lines, write_line'access);
			iterate (packge.silk_screen.top.arcs, write_arc'access);
			iterate (packge.silk_screen.top.circles, write_circle'access);
			iterate (packge.silk_screen.top.polygons, write_polygon'access);
			iterate (packge.silk_screen.top.texts, write_text'access);
			iterate (packge.silk_screen.top.placeholders, write_placeholder'access);
			section_mark (section_top, FOOTER);
			
			-- bottom
			section_mark (section_bottom, HEADER);
			iterate (packge.silk_screen.bottom.lines, write_line'access);
			iterate (packge.silk_screen.bottom.arcs, write_arc'access);
			iterate (packge.silk_screen.bottom.circles, write_circle'access);
			iterate (packge.silk_screen.bottom.polygons, write_polygon'access);
			iterate (packge.silk_screen.bottom.texts, write_text'access);
			iterate (packge.silk_screen.bottom.placeholders, write_placeholder'access);
			section_mark (section_bottom, FOOTER);

			section_mark (section_silk_screen, FOOTER);			
		end write_silk_screen;

		procedure write_assembly_documentation is begin
			section_mark (section_assembly_doc, HEADER);

			-- top
			section_mark (section_top, HEADER);
			iterate (packge.assembly_documentation.top.lines, write_line'access);
			iterate (packge.assembly_documentation.top.arcs, write_arc'access);
			iterate (packge.assembly_documentation.top.circles, write_circle'access);
			iterate (packge.assembly_documentation.top.polygons, write_polygon'access);
			iterate (packge.assembly_documentation.top.texts, write_text'access);
			iterate (packge.assembly_documentation.top.placeholders, write_placeholder'access);
			section_mark (section_top, FOOTER);
			
			-- bottom
			section_mark (section_bottom, HEADER);
			iterate (packge.assembly_documentation.bottom.lines, write_line'access);
			iterate (packge.assembly_documentation.bottom.arcs, write_arc'access);
			iterate (packge.assembly_documentation.bottom.circles, write_circle'access);
			iterate (packge.assembly_documentation.bottom.polygons, write_polygon'access);
			iterate (packge.assembly_documentation.bottom.texts, write_text'access);
			iterate (packge.assembly_documentation.bottom.placeholders, write_placeholder'access);
			section_mark (section_bottom, FOOTER);

			section_mark (section_assembly_doc, FOOTER);
		end write_assembly_documentation;
		
		procedure write_keepout is begin
			section_mark (section_keepout, HEADER);

			-- top
			section_mark (section_top, HEADER);
			iterate (packge.keepout.top.lines, write_line'access);
			iterate (packge.keepout.top.arcs, write_arc'access);
			iterate (packge.keepout.top.circles, write_circle'access);
			iterate (packge.keepout.top.polygons, write_polygon'access);			
			section_mark (section_top, FOOTER);
			
			-- bottom
			section_mark (section_bottom, HEADER);
			iterate (packge.keepout.bottom.lines, write_line'access);
			iterate (packge.keepout.bottom.arcs, write_arc'access);
			iterate (packge.keepout.bottom.circles, write_circle'access);
			iterate (packge.keepout.bottom.polygons, write_polygon'access);			
			section_mark (section_bottom, FOOTER);

			section_mark (section_keepout, FOOTER);			
		end write_keepout;

		procedure write_stop_mask is begin
			section_mark (section_stop_mask, HEADER);

			-- top
			section_mark (section_top, HEADER);
			iterate (packge.stop_mask.top.lines, write_line'access);
			iterate (packge.stop_mask.top.arcs, write_arc'access);
			iterate (packge.stop_mask.top.circles, write_circle'access);
			iterate (packge.stop_mask.top.polygons, write_polygon'access);			
			section_mark (section_top, FOOTER);
			
			-- bottom
			section_mark (section_bottom, HEADER);
			iterate (packge.stop_mask.bottom.lines, write_line'access);
			iterate (packge.stop_mask.bottom.arcs, write_arc'access);
			iterate (packge.stop_mask.bottom.circles, write_circle'access);
			iterate (packge.stop_mask.bottom.polygons, write_polygon'access);			
			section_mark (section_bottom, FOOTER);

			section_mark (section_stop_mask, FOOTER);			
		end write_stop_mask;

		procedure write_stencil is begin
			section_mark (section_stencil, HEADER);

			-- top
			section_mark (section_top, HEADER);
			iterate (packge.stencil.top.lines, write_line'access);
			iterate (packge.stencil.top.arcs, write_arc'access);
			iterate (packge.stencil.top.circles, write_circle'access);
			iterate (packge.stencil.top.polygons, write_polygon'access);			
			section_mark (section_top, FOOTER);
			
			-- bottom
			section_mark (section_bottom, HEADER);
			iterate (packge.stencil.bottom.lines, write_line'access);
			iterate (packge.stencil.bottom.arcs, write_arc'access);
			iterate (packge.stencil.bottom.circles, write_circle'access);
			iterate (packge.stencil.bottom.polygons, write_polygon'access);			
			section_mark (section_bottom, FOOTER);

			section_mark (section_stencil, FOOTER);			
		end write_stencil;

		procedure write_route_restrict is begin
			section_mark (section_route_restrict, HEADER);

			iterate (packge.route_restrict.lines, write_line'access);
			iterate (packge.route_restrict.arcs, write_arc'access);
			iterate (packge.route_restrict.circles, write_circle'access);
			iterate (packge.route_restrict.polygons, write_polygon'access);			

			section_mark (section_route_restrict, FOOTER);			
		end write_route_restrict;

		procedure write_via_restrict is begin
			section_mark (section_via_restrict, HEADER);

			iterate (packge.via_restrict.lines, write_line'access);
			iterate (packge.via_restrict.arcs, write_arc'access);
			iterate (packge.via_restrict.circles, write_circle'access);
			iterate (packge.via_restrict.polygons, write_polygon'access);			

			section_mark (section_via_restrict, FOOTER);			
		end write_via_restrict;

		procedure write_contour is begin
			section_mark (section_pcb_contour, HEADER);

			iterate (packge.pcb_contour.lines, write_line'access);
			iterate (packge.pcb_contour.arcs, write_arc'access);
			iterate (packge.pcb_contour.circles, write_circle'access);

			section_mark (section_pcb_contour, FOOTER);
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
			section_mark (section_pac_3d_contour, HEADER);
			-- CS
			section_mark (section_pac_3d_contour, FOOTER);
		end write_package_contour;

		procedure write_terminals is 
			use type_terminals;
			terminal_cursor : type_terminals.cursor := packge.terminals.first;

			procedure write_pad_shape (pad_shape : in type_pad_outline) is
				use type_pad_lines;
				use type_pad_arcs;
				use type_pad_circles;
				use type_pad_polygons;

				procedure write_line (cursor : in type_pad_lines.cursor) is begin
					line_begin;
					write (keyword => keyword_start, parameters => position (element (cursor).start_point));
					write (keyword => keyword_end  , parameters => position (element (cursor).end_point));
					line_end;
				end write_line;

				procedure write_arc (cursor : in type_pad_arcs.cursor) is begin
					arc_begin;
					write (keyword => keyword_center, parameters => position (element (cursor).center));
					write (keyword => keyword_start, parameters => position (element (cursor).start_point));
					write (keyword => keyword_end  , parameters => position (element (cursor).end_point));
					arc_end;
				end write_arc;

				procedure write_circle (cursor : in type_pad_circles.cursor) is begin
					circle_begin;
					write (keyword => keyword_center, parameters => position (element (cursor).center));
					write (keyword => keyword_radius, parameters => et_pcb_coordinates.to_string (element (cursor).radius));
					circle_end;
				end write_circle;

				procedure write_polygon (cursor : in type_pad_polygons.cursor) is 
					procedure query_points (polygon : in type_pad_polygon) is 
						use et_pcb.type_polygon_points;
					begin
						iterate (polygon.points, write_polygon_corners'access);
					end query_points;
				begin -- write_polygon
					polygon_begin;
					query_element (cursor, query_points'access);
					polygon_end;
				end write_polygon;
									
			begin --write_pad_shape
				iterate (pad_shape.lines, write_line'access);
				iterate (pad_shape.arcs, write_arc'access);
				iterate (pad_shape.circles, write_circle'access);
				iterate (pad_shape.polygons, write_polygon'access);
			end write_pad_shape;

			procedure write_plated_millings (millings : in type_package_pcb_contour_plated) is
			begin
				section_mark (section_pad_millings, HEADER);
				iterate (millings.lines, write_line'access);
				iterate (millings.arcs, write_arc'access);
				iterate (millings.circles, write_circle'access);
				section_mark (section_pad_millings, FOOTER);
			end write_plated_millings;
			
		begin -- write_terminals
			section_mark (section_terminals, HEADER);
			
			while terminal_cursor /= type_terminals.no_element loop
				section_mark (section_terminal, HEADER);
				write (keyword => keyword_name, parameters => et_libraries.to_string (key (terminal_cursor)));
				write (keyword => keyword_assembly_technology, parameters => to_string (element (terminal_cursor).technology));
				write (keyword => keyword_position, parameters => position (element (terminal_cursor).position));
				
				case element (terminal_cursor).technology is
					when THT =>
						-- pad contour top
						section_mark (section_pad_contour, HEADER);
						
						section_mark (section_top, HEADER);
						write_pad_shape (element (terminal_cursor).pad_shape_top);
						section_mark (section_top, FOOTER);

						-- pad contour bottom
						section_mark (section_bottom, HEADER);
						write_pad_shape (element (terminal_cursor).pad_shape_bottom);
						section_mark (section_bottom, FOOTER);
						
						section_mark (section_pad_contour, FOOTER);

						-- copper width in inner layers
						write (keyword => keyword_width_inner_layers, 
							   parameters => et_pcb_coordinates.to_string (element (terminal_cursor).width_inner_layers));
						
						-- A THT terminal can have a drilled or a milled hole:
						write (keyword => keyword_tht_hole, parameters => to_string (element (terminal_cursor).tht_hole));

						case element (terminal_cursor).tht_hole is
							when DRILLED => 
								write (keyword_drill_size, parameters => et_pcb_coordinates.to_string (element (terminal_cursor).drill_size));
								
							when MILLED => 
								write_plated_millings (element (terminal_cursor).millings);
						end case;
						
					when SMT =>
						-- pad contour
						section_mark (section_pad_contour, HEADER);
						write_pad_shape (element (terminal_cursor).pad_shape);
						section_mark (section_pad_contour, FOOTER);
						
						write (keyword => keyword_face, parameters => et_pcb_coordinates.to_string (element (terminal_cursor).face));
						write (keyword => keyword_stop_mask, parameters => et_pcb.to_string (element (terminal_cursor).stop_mask));
						write (keyword => keyword_solder_paste, parameters => et_pcb.to_string (element (terminal_cursor).solder_paste));	
				end case;


				section_mark (section_terminal, FOOTER);
				next (terminal_cursor);
			end loop;
			
			section_mark (section_terminals, FOOTER);
		end write_terminals;
		
	begin -- save_package
		log (name, log_threshold);

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
			log (ada.exceptions.exception_message (event));
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
	
	procedure open_project (log_threshold : in et_string_processing.type_log_level) is
	-- Opens and reads the schematic and layout data present in project file (project_file_handle).
		use et_string_processing;
		use ada.directories;

		function f (line : in type_fields_of_line; position : in positive) return string 
			renames et_string_processing.field;
		
		-- We need a backup of the current working directory. When this procedure finishes,
		-- the working directory must restored.
		current_working_directory : string := current_directory;

		function write_top_level_reached return string is begin return "top level reached"; end;
		function write_enter_section return string is begin return "entering section "; end;
		function write_return_to_section return string is begin return "returning to section "; end;
		function write_missing_begin_end return string is begin 
			return "missing " & section_begin & " or " & section_end & " after section name !"; end;
		function write_section_stack_not_empty return string is begin
			return "section stack not empty !"; end;

		procedure invalid_section is begin
			log_indentation_reset;
			log (message_error & "invalid section name !", console => true);
			raise constraint_error;
		end;

		procedure invalid_keyword (word : in string) is begin
			log_indentation_reset;
			log (message_error & "invalid keyword '" & word & "' !", console => true);
			raise constraint_error;
		end;

		procedure expect_field_count (line : in type_fields_of_line; count_expected : in count_type) is 
			count_found : count_type := field_count (line);
			f1 : string := f (line, 1); -- CS: line must have at least one field otherwise exception occurs here
		begin
			if count_found = count_expected then null; -- fine, field count as expected
			
			elsif count_found < count_expected then -- less fields than expected
				log_indentation_reset;
				log (message_error & "missing parameter for '" & f1 & "' !", console => true);
				raise constraint_error;
				
			elsif count_found > count_expected then -- more fields than expeced
				log (message_warning & affected_line (line) & "excessive parameters after '" 
					 & f (line, positive (count_expected)) & "' ignored !");
			end if;
			
		end expect_field_count;



	-- MODULES
		
		-- The search of rig module files requires this stuff:
		module_file_search : search_type; -- the state of the search
		module_file_filter : filter_type := (ordinary_file => true, others => false);

		procedure read_module_file (module_file_handle : in directory_entry_type) is 
			file_handle : ada.text_io.file_type;
			file_name : string := simple_name (module_file_handle); -- motor_driver.mod
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

			function to_position ( -- CS combine with next function to_position using the tag test ?									 
				line : in type_fields_of_line; -- "start x 3 y 4" or "position x 44.5 y 53.5"
				from : in positive)
				return et_coordinates.type_2d_point is
				use et_coordinates;
				
				point : type_2d_point; -- to be returned

				place : positive := from; -- the field being read from given line

				-- CS: flags to detect missing x or y
			begin
				while place <= positive (field_count (line)) loop

					-- We expect after the x the corresponding value for x
					if f (line, place) = keyword_pos_x then
						set_x (point, to_distance (f (line, place + 1)));

					-- We expect after the y the corresponding value for y
					elsif f (line, place) = keyword_pos_y then
						set_y (point, to_distance (f (line, place + 1)));

					else
						invalid_keyword (f (line, place));
					end if;
						
					place := place + 2;
				end loop;
				
				return point;
			end to_position;

			function to_position (
				line : in type_fields_of_line; -- "position sheet 3 x 44.5 y 53.5"
				from : in positive)
				return et_coordinates.type_coordinates is
				use et_coordinates;
				
				point : type_coordinates; -- to be returned

				place : positive := from; -- the field being read from given line

				-- CS: flags to detect missing sheet, x or y
			begin
				while place <= positive (field_count (line)) loop

					-- We expect after "sheet" the sheet number
					if f (line, place) = keyword_sheet then
						set_sheet (point, to_sheet_number (f (line, place + 1)));
						
					-- We expect after the x the corresponding value for x
					elsif f (line, place) = keyword_pos_x then
						set_x (point, to_distance (f (line, place + 1)));

					-- We expect after the y the corresponding value for y
					elsif f (line, place) = keyword_pos_y then
						set_y (point, to_distance (f (line, place + 1)));

					else
						invalid_keyword (f (line, place));
					end if;
						
					place := place + 2;
				end loop;
				
				return point;
			end to_position;


			function to_position (
			-- Returns a type_point_2d in the the layout.
				line : in type_fields_of_line; -- "start x 44.5 y 53.5"
				from : in positive)
				return et_pcb_coordinates.type_point_2d is
				use et_pcb_coordinates;
				
				point : type_point_2d; -- to be returned

				place : positive := from; -- the field being read from given line

				-- CS: flags to detect missing sheet, x or y
			begin
				while place <= positive (field_count (line)) loop

					-- We expect after the x the corresponding value for x
					if f (line, place) = keyword_pos_x then
						set_point (point => point, axis => X, value => to_distance (f (line, place + 1)));

					-- We expect after the y the corresponding value for y
					elsif f (line, place) = keyword_pos_y then
						set_point (point => point, axis => Y, value => to_distance (f (line, place + 1)));

					else
						invalid_keyword (f (line, place));
					end if;
						
					place := place + 2;
				end loop;
				
				return point;
			end to_position;
				
			
			-- VARIABLES FOR TEMPORARILY STORAGE AND ASSOCIATED HOUSEKEEPING SUBPROGRAMS:

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
			net_name	: et_schematic.type_net_name.bounded_string; -- motor_on_off
			net			: et_schematic.type_net;

			procedure reset_net is begin
				net := (others => <>);
			end reset_net;

			strands : et_schematic.type_strands.list;
			strand	: et_schematic.type_strand;
			net_segments : et_schematic.type_net_segments.list;
			net_segment	: et_schematic.type_net_segment;
			
			net_labels : et_schematic.type_net_labels.list;
			net_label : et_schematic.type_net_label_base;
			net_label_appearance : et_schematic.type_net_label_appearance := et_schematic.type_net_label_appearance'first;

			-- The net label direction is relevant if appearance is TAG:
			net_label_direction : et_schematic.type_net_label_direction := et_schematic.type_net_label_direction'first;
			
			net_junction : et_schematic.type_net_junction;
			net_junctions : et_schematic.type_junctions.list;

			net_port : et_schematic.type_port_component;
			net_ports : et_schematic.type_ports_component.list;

			net_submodule_port : et_schematic.type_port_submodule;
			net_submodule_ports : et_schematic.type_ports_submodule.list;
			
			route_line 		: et_pcb.type_copper_line_pcb;
			route_arc		: et_pcb.type_copper_arc_pcb;
			route_via		: et_pcb.type_via;

-- 			route_polygon_priority_level	: et_pcb.type_polygon_priority := et_pcb.type_polygon_priority'first;
-- 			route_polygon_isolation_gap		: et_pcb.type_track_clearance := et_pcb.type_track_clearance'first;
			route_polygon					: et_pcb.type_copper_polygon;
			route_polygon_pad_connection	: et_pcb.type_polygon_pad_connection := et_pcb.type_polygon_pad_connection'first;
			route_polygon_layer				: et_pcb.type_signal_layer;
			route_polygon_pad_technology	: et_pcb.type_polygon_pad_technology := et_pcb.type_polygon_pad_technology'first;
			route_polygon_thermal_width		: et_pcb.type_polygon_thermal_width := et_pcb.type_polygon_thermal_width'first;
			route_polygon_thermal_gap		: et_pcb.type_polygon_thermal_gap := et_pcb.type_polygon_thermal_gap'first;
			route_polygon_solid_technology	: et_pcb.type_polygon_pad_technology := et_pcb.type_polygon_pad_technology'first;
			
			procedure process_line is 
			-- CS: detect if section name is type_section_name_module

				procedure execute_section is
				-- Once a section concludes, the temporarily variables are read, evaluated
				-- and finally assembled to actual objects:

					procedure insert_net_class (
						module_name	: in et_coordinates.type_submodule_name.bounded_string;
						module		: in out et_schematic.type_module) is
						use et_pcb;
						inserted : boolean;
						cursor : type_net_classes.cursor;
					begin -- insert_net_class
						log ("net class " & to_string (net_class_name), log_threshold + 2);

						-- CS: notify about missing parameters (by reading the parameter-found-flags)
						-- If a parameter is missing, the default is assumed. See type_net_class spec.
						
						type_net_classes.insert (
							container	=> module.net_classes,
							key			=> net_class_name,
							new_item	=> net_class,
							inserted	=> inserted,
							position	=> cursor);

						if not inserted then
							log_indentation_reset;
							log (message_error & "net class '" & et_pcb.to_string (net_class_name) 
								 & "' already exists !", console => true);
							raise constraint_error;
						end if;

						reset_net_class; -- clean up for next net class
						
					end insert_net_class;

					procedure insert_net (
						module_name	: in et_coordinates.type_submodule_name.bounded_string;
						module		: in out et_schematic.type_module) is
						use et_schematic;
						inserted : boolean;
						cursor : type_nets.cursor;
					begin -- insert_net
						log ("net " & to_string (net_name), log_threshold + 2);

						-- CS: notify about missing parameters (by reading the parameter-found-flags)
						-- If a parameter is missing, the default is assumed. See type_net spec.
						
						type_nets.insert (
							container	=> module.nets,
							key			=> net_name,
							new_item	=> net,
							inserted	=> inserted,
							position	=> cursor);

						if not inserted then
							log_indentation_reset;
							log (message_error & "net '" & to_string (net_name) 
								 & "' already exists !", console => true);
							raise constraint_error;
						end if;

						reset_net; -- clean up for next net
						
					end insert_net;
					

					
				begin -- execute_section
					case stack.parent is
						when SEC_NET_CLASSES =>
							case stack.current is
								when SEC_NET_CLASS =>

									-- insert net class
									update_element (
										container	=> modules,
										position	=> module_cursor,
										process		=> insert_net_class'access);
									
								when others => invalid_section;
							end case;

						when SEC_NETS =>
							case stack.current is
								when SEC_NET =>

									-- insert net
									update_element (
										container	=> modules,
										position	=> module_cursor,
										process		=> insert_net'access);

								when others => invalid_section;
							end case;

						when SEC_NET =>
							case stack.current is
								when SEC_STRANDS =>

									-- insert strand collection in net
									net.strands := strands;
									et_schematic.type_strands.clear (strands); -- clean up for next strand collection

								when SEC_ROUTE =>
									null; -- CS
									
								when others => invalid_section;
							end case;

						when SEC_STRANDS =>
							case stack.current is
								when SEC_STRAND =>

									-- insert strand in strands
									et_schematic.type_strands.append (
										container	=> strands,
										new_item	=> strand);

									-- clean up for next single strand
									strand := (others => <>); 
									
								when others => invalid_section;
							end case;

						when SEC_STRAND =>
							case stack.current is
								when SEC_SEGMENTS =>

									-- insert segments in strand
									strand.segments := net_segments;

									-- clean up for next segment collection
									et_schematic.type_net_segments.clear (net_segments);
									
								when others => invalid_section;
							end case;

						when SEC_SEGMENTS =>
							case stack.current is
								when SEC_SEGMENT =>

									-- insert segment in segment collection
									et_schematic.type_net_segments.append (
										container	=> net_segments,
										new_item	=> net_segment);

									-- clean up for next segment
									net_segment := (others => <>);
									
								when others => invalid_section;
							end case;

						when SEC_SEGMENT =>
							case stack.current is
								when SEC_LABELS =>

									-- insert labels in segment
									net_segment.labels := net_labels;

									-- clean up for next label collection
									et_schematic.type_net_labels.clear (net_labels);

								when SEC_JUNCTIONS =>
									-- NOTE: A junction is defined by a single line.
									-- Upon reading the line like "position x 4 y 4" the junction is
									-- appended to the junction collection immediately when the line 
									-- is read. See main code of process_line.
									-- There is no section for a single junction like [JUNCTION BEGIN].

									-- insert junction collection in segment
									net_segment.junctions := net_junctions;

									-- clean up for next junction collection (of another net segment)
									et_schematic.type_junctions.clear (net_junctions);
									
								when SEC_PORTS =>
									-- NOTE: A device port is defined by a single line.
									-- Upon reading the line like "device R3 port 1" the port is
									-- appended to the port collection immediately when the line 
									-- is read. See main code of process_line.
									-- There is no section for a single device port like [PORT BEGIN].

									-- insert port collection in segment
									net_segment.component_ports := net_ports;

									-- clean up for next port collection (of another net segment)
									et_schematic.type_ports_component.clear (net_ports);

								when SEC_SUBMODULE_PORTS =>
									-- insert submodule ports in segment
									net_segment.submodule_ports := net_submodule_ports;
									
								when others => invalid_section;
							end case;

						when SEC_LABELS =>
							case stack.current is
								when SEC_LABEL =>

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

						when SEC_SUBMODULE_PORTS =>
							case stack.current is
								when SEC_PORT =>

									-- insert submodule port in collection of submodule ports
									et_schematic.type_ports_submodule.append (net_submodule_ports, net_submodule_port);

									-- clean up for next submodule port
									net_submodule_port := (others => <>);

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
					section_keyword	: in string; -- [NETS
					section			: in type_section_name_module) -- SEC_NETS
					return boolean is 
				begin -- set
					if f (line, 1) = section_keyword then -- section name detected in field 1
						if f (line, 2) = section_begin then -- section header detected in field 2
							stack.push (section);
							log (write_enter_section & to_string (section), log_threshold + 3);
							return true;

						elsif f (line, 2) = section_end then -- section footer detected in field 2

							-- Now that the section ends, the data collected in temporarily
							-- variables is processed.
							execute_section;
							
							stack.pop;
							if stack.empty then
								log (write_top_level_reached, log_threshold + 3);
							else
								log (write_return_to_section & to_string (stack.current), log_threshold + 3);
							end if;
							return true;

						else
							log_indentation_reset;
							log (message_error & write_missing_begin_end, console => true);
							raise constraint_error;
						end if;

					else -- neither a section header nor footer
						return false;
					end if;
				end set;


			begin -- process_line
				if set (section_net_classes, SEC_NET_CLASSES) then null;
				elsif set (section_net_class, SEC_NET_CLASS) then null;
				elsif set (section_nets, SEC_NETS) then null;
				elsif set (section_net, SEC_NET) then null;
				elsif set (section_strands, SEC_STRANDS) then null;
				elsif set (section_strand, SEC_STRAND) then null;
				elsif set (section_segments, SEC_SEGMENTS) then null;
				elsif set (section_segment, SEC_SEGMENT) then null;
				elsif set (section_labels, SEC_LABELS) then null;
				elsif set (section_label, SEC_LABEL) then null;
				elsif set (section_junctions, SEC_JUNCTIONS) then null;
				elsif set (section_ports, SEC_PORTS) then null;
				elsif set (section_submodule_ports, SEC_SUBMODULE_PORTS) then null;								
				elsif set (section_ports, SEC_PORT) then null;				
				elsif set (section_route, SEC_ROUTE) then null;								
				elsif set (section_line, SEC_LINE) then null;								
				elsif set (section_arc, SEC_ARC) then null;								
				elsif set (section_polygon, SEC_POLYGON) then null;								
				elsif set (section_corners, SEC_CORNERS) then null;								
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
				elsif set (section_pcb_contour, SEC_PCB_CONTOUR_NON_PLATED) then null;
				else
					-- The line contains something else -> the payload data. 
					-- Temporarily this data is stored in corresponding variables.

					log ("line --> " & to_string (line), log_threshold + 3);

					case stack.parent is
						when SEC_INIT => -- test for allowed sections at top level
							case stack.current is
								when SEC_NET_CLASSES => null; -- nothing to do
								when SEC_NETS => null; -- nothing to do
								when SEC_TEXTS => null; -- nothing to do
								when SEC_DEVICES => null; -- nothing to do
								when SEC_SUBMODULES => null; -- nothing to do
								when SEC_BOARD => null; -- nothing to do
								when others => invalid_section;
							end case;
						
						when SEC_NET_CLASSES =>
							case stack.current is
								when SEC_NET_CLASS =>
									declare
										kw : string := f (line, 1);
									begin
										if kw = keyword_name then
											expect_field_count (line, 2);
											net_class_name := et_pcb.to_net_class_name (f (line,2));

										-- CS: In the following: set a corresponding parameter-found-flag
										elsif kw = keyword_description then
											expect_field_count (line, 2);
											net_class.description := et_pcb.to_net_class_description (f (line,2));
											
										elsif kw = keyword_clearance then
											expect_field_count (line, 2);
											net_class.clearance := et_pcb_coordinates.to_distance (f (line,2));
											et_pcb.validate_track_clearance (net_class.clearance);
											
										elsif kw = keyword_track_width_min then
											expect_field_count (line, 2);
											net_class.track_width_min := et_pcb_coordinates.to_distance (f (line,2));
											et_pcb.validate_track_width (net_class.track_width_min);
											
										elsif kw = keyword_via_drill_min then
											expect_field_count (line, 2);
											net_class.via_drill_min := et_pcb_coordinates.to_distance (f (line,2));
											et_pcb.validate_drill_size (net_class.via_drill_min);
											
										elsif kw = keyword_via_restring_min then
											expect_field_count (line, 2);
											net_class.via_restring_min := et_pcb_coordinates.to_distance (f (line,2));
											et_pcb.validate_restring_width (net_class.via_restring_min);
											
										elsif kw = keyword_micro_via_drill_min then
											expect_field_count (line, 2);
											net_class.micro_via_drill_min := et_pcb_coordinates.to_distance (f (line,2));
											et_pcb.validate_drill_size (net_class.micro_via_drill_min);
											
										elsif kw = keyword_micro_via_restring_min then
											expect_field_count (line, 2);
											net_class.micro_via_restring_min := et_pcb_coordinates.to_distance (f (line,2));
											et_pcb.validate_restring_width (net_class.micro_via_restring_min);
										else
											invalid_keyword (kw);
										end if;
									end;

								when others => invalid_section;
							end case;

						when SEC_NET => -- test for allowed sections in section NET
							case stack.current is
								when SEC_STRANDS | SEC_ROUTE => null;
								when others => invalid_section;
							end case;
							
						when SEC_NETS =>
							case stack.current is
								when SEC_NET =>
									declare
										kw : string := f (line, 1);
									begin
										-- CS: In the following: set a corresponding parameter-found-flag
										if kw = keyword_name then
											expect_field_count (line, 2);
											net_name := et_schematic.to_net_name (f (line,2));
											
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
												log (message_warning & affected_line (line) & "No net class specified ! Assume default class !");
											end if;
										elsif kw = keyword_scope then
											expect_field_count (line, 2);
											net.scope := et_schematic.to_net_scope (f (line,2));
											
										else
											invalid_keyword (kw);
										end if;
									end;

											
								when others => invalid_section;
							end case;

						when SEC_SEGMENTS =>
							case stack.current is
								when SEC_SEGMENT =>
									declare
										kw : string := f (line, 1);
									begin
										-- CS: In the following: set a corresponding parameter-found-flag
										if kw = keyword_start then -- "start x 3 y 4"
											expect_field_count (line, 7);

											-- extract start position starting at field 2
											net_segment.coordinates_start := to_position (line, from => 2);
											
										elsif kw = keyword_end then -- "end x 6 y 4"
											expect_field_count (line, 7);

											-- extract end position starting at field 2
											net_segment.coordinates_end := to_position (line, from => 2);
											
										else
											invalid_keyword (kw);
										end if;
									end;
											
								when others => invalid_section;
							end case;

						when SEC_SEGMENT =>
							case stack.current is
								when SEC_LABELS => null; -- nothing to do
								when SEC_JUNCTIONS =>
									-- read junction parameters
									-- NOTE: A junction is defined by a single line.
									-- Upon reading the line like "position x 4 y 4" the junction is
									-- appended to the junction collection immediately here. See procdure
									-- execute_section.
									-- There is no section for a single junction like [JUNCTION BEGIN].
									declare
										kw : string := f (line, 1);
									begin
										if kw = keyword_position then
											expect_field_count (line, 5);

											-- extract position of junction starting at field 2
											net_junction.coordinates := to_position (line, from => 2);

											-- append junction to junction collection
											et_schematic.type_junctions.append (net_junctions, net_junction);
										else
											invalid_keyword (kw);
										end if;
									end;
									
								when SEC_PORTS =>
									-- read device port parameters
									-- NOTE: A device port is defined by a single line.
									-- Upon reading the line like "device IC3 port CE" the port is
									-- appended to the port collection immediately here. See procdure
									-- execute_section.									
									-- There is no section for a single device port like [PORT BEGIN].
									declare
										kw : string := f (line, 1);
									begin
										if kw = keyword_device then
											expect_field_count (line, 4);

											net_port.reference := et_schematic.to_component_reference (f (line, 2)); -- IC3

											if f (line, 3) = keyword_port then -- port
												net_port.name := et_libraries.to_port_name (f (line, 4)); -- CE

												-- append port to port collection
												et_schematic.type_ports_component.append (net_ports, net_port); 
											else
												invalid_keyword (f (line, 3));
											end if;
										else
											invalid_keyword (kw);
										end if;
									end;

								when SEC_SUBMODULE_PORTS => null; -- nothing to do
								when others => invalid_section;
							end case;

						when SEC_LABELS =>
							case stack.current is
								when SEC_LABEL =>

									declare
										kw : string := f (line, 1);
									begin
										-- CS: In the following: set a corresponding parameter-found-flag
										if kw = keyword_position then -- position x 148.59 y 104.59
											expect_field_count (line, 5);

											-- extract label position starting at field 2 of line
											net_label.coordinates := to_position (line, 2);
											
										elsif kw = keyword_rotation then -- rotation 0.0
											expect_field_count (line, 2);
											net_label.orientation := et_coordinates.to_angle (f (line, 2));

										elsif kw = keyword_size then -- size 1.3
											expect_field_count (line, 2);
											net_label.size := et_coordinates.to_distance (f (line, 2));

										elsif kw = keyword_style then -- style normal
											expect_field_count (line, 2);
											net_label.style := et_libraries.to_text_style (f (line, 2));

										elsif kw = keyword_line_width then -- line_width 0.1
											expect_field_count (line, 2);
											net_label.width := et_coordinates.to_distance (f (line, 2));

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

						when SEC_SUBMODULE_PORTS =>
							case stack.current is
								when SEC_PORT =>
									declare
										kw : string := f (line, 1);
									begin
										-- CS: In the following: set a corresponding parameter-found-flag
										if kw = keyword_module then -- module motor_driver
											expect_field_count (line, 2);
											net_submodule_port.module := et_coordinates.to_submodule_name (f (line, 2));
											
										elsif kw = keyword_name then -- name MASTER_RESET_N
											expect_field_count (line, 2);
											net_submodule_port.port := et_libraries.to_port_name (f (line, 2));

										elsif kw = keyword_position then -- position x 3 y 4
											expect_field_count (line, 5);

											-- extract port position starting at field 2
											net_submodule_port.position := to_position (line, 2); 

										elsif kw = keyword_direction then -- direction input
											expect_field_count (line, 2);
											net_submodule_port.direction := et_libraries.to_port_direction (f (line, 2));

										else
											invalid_keyword (kw);
										end if;
									end;

								when others => invalid_section;
							end case;

						when SEC_ROUTE =>
							case stack.current is
								when SEC_LINE =>
									declare
										kw : string := f (line, 1);
									begin
										-- CS: In the following: set a corresponding parameter-found-flag
										if kw = keyword_start then -- start x 22.3 y 23.3
											expect_field_count (line, 5);

											-- extract the start position starting at field 2 of line
											route_line.start_point := to_position (line, 2);
											
										elsif kw = keyword_end then -- end x 22.3 y 23.3
											expect_field_count (line, 5);

											-- extract the end position starting at field 2 of line
											route_line.end_point := to_position (line, 2);

										elsif kw = keyword_layer then -- layer 2
											expect_field_count (line, 2);
											route_line.layer := et_pcb.to_signal_layer (f (line, 2));

										elsif kw = keyword_width then -- width 0.5
											expect_field_count (line, 2);
											route_line.width := et_pcb_coordinates.to_distance (f (line, 2));
											
										else
											invalid_keyword (kw);
										end if;
									end;


								when SEC_ARC =>
									declare
										kw : string := f (line, 1);
									begin
										-- CS: In the following: set a corresponding parameter-found-flag
										if kw = keyword_start then -- start x 22.3 y 23.3
											expect_field_count (line, 5);

											-- extract the start position starting at field 2 of line
											route_arc.start_point := to_position (line, 2);

										elsif kw = keyword_end then -- end x 22.3 y 23.3
											expect_field_count (line, 5);

											-- extract the end position starting at field 2 of line
											route_arc.end_point := to_position (line, 2);
											
										elsif kw = keyword_center then -- center x 22.3 y 23.3
											expect_field_count (line, 5);

											-- extract the center position starting at field 2 of line
											route_arc.center := to_position (line, 2);

										elsif kw = keyword_layer then -- layer 2
											expect_field_count (line, 2);
											route_arc.layer := et_pcb.to_signal_layer (f (line, 2));

										elsif kw = keyword_width then -- width 0.5
											expect_field_count (line, 2);
											route_arc.width := et_pcb_coordinates.to_distance (f (line, 2));
											
										else
											invalid_keyword (kw);
										end if;
									end;


								when SEC_POLYGON =>
									declare
										kw : string := f (line, 1);
									begin
										-- CS: In the following: set a corresponding parameter-found-flag
										if kw = keyword_priority then -- priority 2
											expect_field_count (line, 2);
											route_polygon.priority_level := et_pcb.to_polygon_priority (f (line, 2));

										elsif kw = keyword_isolation then -- isolation 0.5
											expect_field_count (line, 2);
											route_polygon.isolation_gap := et_pcb_coordinates.to_distance (f (line, 2));
											
										elsif kw = keyword_corner_easing then -- corner_easing none/chamfer/fillet
											expect_field_count (line, 2);
											route_polygon.corner_easing := et_pcb.to_corner_easing (f (line, 2));

										elsif kw = keyword_easing_radius then -- easing_radius 0.3
											expect_field_count (line, 2);
											route_polygon.easing_radius := et_pcb_coordinates.to_distance (f (line, 2));

										elsif kw = keyword_fill_style then -- fill_style solid,hatched,cutout
											expect_field_count (line, 2);
											route_polygon.fill_style := et_pcb.to_fill_style (f (line, 2));

										elsif kw = keyword_hatching_line_width then -- hatching_line_width 1
											expect_field_count (line, 2);
											route_polygon.hatching_line_width := et_pcb_coordinates.to_distance (f (line, 2));

										elsif kw = keyword_hatching_line_spacing then -- hatching_line_spacing 1
											expect_field_count (line, 2);
											route_polygon.hatching_spacing := et_pcb_coordinates.to_distance (f (line, 2));

										elsif kw = keyword_layer then -- layer 2
											expect_field_count (line, 2);
											route_polygon.hatching_spacing := et_pcb_coordinates.to_distance (f (line, 2));

											-- 											
-- 										else
-- 											invalid_keyword (kw);
										end if;
									end;


								when SEC_VIA => null;

								when others => invalid_section;
							end case;
							
						when SEC_DRAWING_FRAMES =>
							NULL;

						when SEC_TEXTS =>
							NULL;
							
						when SEC_SUBMODULES =>
							NULL;
							
						when SEC_DEVICES =>
							NULL;

						when SEC_BOARD =>
							NULL;
							
						when others => null;
					end case;
				end if;

				exception when event: others =>
					log (affected_line (line) & to_string (line), console => true);
					raise;
				
			end process_line;

			
		begin -- read_module_file
			-- write name of configuration file
			log (file_name, log_threshold + 1);
			log_indentation_up;

			-- open module file
			open (
				file => file_handle,
				mode => in_file, 
				name => file_name);

			set_input (file_handle);
			
			-- Init section stack.
			stack.init;
			stack.push (SEC_INIT);

			-- Create an empty module named after the module file.
			type_modules.insert (
				container	=> modules,
				key			=> et_coordinates.to_submodule_name (simple_name (file_name)),
				position	=> module_cursor,
				inserted	=> module_inserted);

			-- read the file line by line
			while not end_of_file loop
				line := et_string_processing.read_line (
					line 			=> get_line,
					number			=> ada.text_io.line (current_input),
					comment_mark 	=> et_string_processing.comment_mark, -- comments start with #
					delimiter_wrap	=> true, -- strings are enclosed in quotations
					ifs 			=> latin_1.space); -- fields are separated by space

				-- we are interested in lines that contain something. emtpy lines are skipped:
				if field_count (line) > 0 then
					process_line;
				end if;
			end loop;

			-- As a safety measure the top section must be reached.
			if stack.depth > 1 then 
				log (message_warning & write_section_stack_not_empty);
			end if;

			log_indentation_down;
			set_input (standard_input);
			close (file_handle);

			exception when event: others =>
				if is_open (file_handle) then close (file_handle); end if;
				raise;
			
		end read_module_file;

		

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
			generic_name : et_coordinates.type_submodule_name.bounded_string; -- motor_driver
			instance_name : type_module_instance_name.bounded_string; -- DRV_1

			procedure clear_module_instance is begin
				generic_name := et_coordinates.type_submodule_name.to_bounded_string ("");
				instance_name := to_bounded_string ("");
			end clear_module_instance;
			
			purpose_A, purpose_B : et_libraries.type_component_purpose.bounded_string; -- power_in, power_out
			instance_A, instance_B : type_module_instance_name.bounded_string; -- DRV_1, PWR

			procedure clear_connector is begin
				purpose_A := et_libraries.to_purpose ("");
				purpose_A := purpose_B;
				instance_A := to_bounded_string ("");
				instance_B := instance_A;
			end clear_connector;
			
			procedure process_line is
			-- CS: detect if section name is type_section_name_rig_configuration

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
							new_item	=> (generic_name => generic_name),
							inserted	=> instance_created,
							position	=> instance_cursor
							);

						-- An instance may exist only once:
						if not instance_created then
							log_indentation_reset;
							log (message_error & "module instance '" 
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
					begin
						-- CS: test length of instance_A/B and purpose A/B. must be greater zero

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
							log_indentation_reset;
							log (message_error & "module connection already exists !", console => true);
							raise constraint_error;
						end if;

						clear_connector; -- clean up for next module connector
					end create_connection;
					
				begin -- execute_section
					case stack.parent is
							
						when SEC_MODULE_INSTANCES =>
							case stack.current is
								when SEC_MODULE =>

									-- create an instanciated module in the rig
									type_rigs.update_element (
										container	=> rigs,
										position	=> rig_cursor,
										process		=> create_instance'access);
									
								when others => invalid_section;
							end case;
							
						when SEC_MODULE_CONNECTIONS =>
							case stack.current is
								when SEC_CONNECTOR =>
									
									-- create a module connector in the rig
									type_rigs.update_element (
										container	=> rigs,
										position	=> rig_cursor,
										process		=> create_connection'access);
									
								when others => invalid_section;
							end case;

						when others => null; --invalid_section;
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
							log (write_enter_section & to_string (section), log_threshold + 4);
							return true;
							
						elsif f (line, 2) = section_end then -- section footer detected in field 2

							-- Now that the section ends, the data collected in temporarily
							-- variables is processed.
							execute_section;
							
							stack.pop;
							if stack.empty then
								log (write_top_level_reached, log_threshold + 4);
							else
								log (write_return_to_section & to_string (stack.current), log_threshold + 4);
							end if;
							return true;
							
						else
							log_indentation_reset;
							log (message_error & write_missing_begin_end, console => true);
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

					log ("line --> " & to_string (line), log_threshold + 3);
					
					case stack.parent is
						when SEC_INIT => -- test for allowed sections at top level
							case stack.current is
								when SEC_MODULE_INSTANCES => null; -- nothing to do
								when SEC_MODULE_CONNECTIONS => null; -- nothing to do
								when others => invalid_section;
							end case;
						
						when SEC_MODULE_INSTANCES =>
							case stack.current is
								when SEC_MODULE =>
									declare
										kw : string := f (line, 1);
									begin
										if kw = keyword_generic_name then
											expect_field_count (line, 2);
											generic_name := to_submodule_name (f (line,2));
											-- CS: test if module with this generic name exists
										elsif kw = keyword_instance_name then
											expect_field_count (line, 2);
											instance_name := to_bounded_string (f (line,2));
										else
											invalid_keyword (kw);
										end if;
									end;
									
								when others => invalid_section;
									
							end case;
							
						when SEC_MODULE_CONNECTIONS =>
							case stack.current is
								when SEC_CONNECTOR =>
									declare
										kw : string := f (line, 1);
									begin
										if kw = keyword_instance_A then
											expect_field_count (line, 2);
											instance_A := to_bounded_string (f (line,2));
											-- CS: test if instance exists
										elsif kw = keyword_instance_B then
											expect_field_count (line, 2);
											instance_B := to_bounded_string (f (line,2));
											-- CS: test if instance exists
											
										elsif kw = keyword_purpose_A then
											expect_field_count (line, 2);
											purpose_A := et_libraries.to_purpose (f (line,2));
											-- CS: test if a connector with this purpose exits in the instance
										elsif kw = keyword_purpose_B then
											expect_field_count (line, 2);
											purpose_B := et_libraries.to_purpose (f (line,2));
											-- CS: test if a connector with this purpose exits in the instance
											
										-- CS: net comparator and warning on/off
										else
											invalid_keyword (kw);
										end if;
									end;
									
								when others => invalid_section;
									
							end case;


						when others => null;
					end case;
				end if;



				exception when event: others =>
					log (affected_line (line) & to_string (line), console => true);
					raise;
				
			end process_line;
			
		begin -- read_conf_file
			-- write name of configuration file
			log (file_name, log_threshold + 1);
			log_indentation_up;

			-- open rig configuration file
			open (
				file => file_handle,
				mode => in_file, 
				name => file_name);

			set_input (file_handle);

			-- Init section stack.
			stack.init;
			stack.push (SEC_INIT);

			-- create an empty rig - named after the given configuration file
			type_rigs.insert (
				container	=> rigs,
				key			=> to_bounded_string (simple_name (file_name)),
				inserted	=> rig_inserted, -- should always be true
				position	=> rig_cursor);
			
			-- read the file line by line
			while not end_of_file loop
				line := et_string_processing.read_line (
					line 			=> get_line,
					number			=> ada.text_io.line (current_input),
					comment_mark 	=> et_string_processing.comment_mark, -- comments start with #
					delimiter_wrap	=> true, -- strings are enclosed in quotations
					ifs 			=> latin_1.space); -- fields are separated by space

				-- we are interested in lines that contain something. emtpy lines are skipped:
				if field_count (line) > 0 then
					process_line;
				end if;
			end loop;

			-- As a safety measure the top section must be reached:
			if stack.depth > 1 then 
				log (message_warning & write_section_stack_not_empty);
			end if;

			log_indentation_down;
			set_input (current_input);
			close (file_handle);

			exception when event: others =>
				if is_open (file_handle) then close (file_handle); end if;
				raise;
			
		end read_conf_file;
	
		
	begin -- open_project
		log ("opening project " & to_string (project_name) & " ...", log_threshold, console => true);
		log_indentation_up;
		
		-- If the given project directory exists, enter it. Otherwise error message and abort.
		if exists (to_string (project_name)) then
			
			-- enter the project directory
			set_directory (to_string (project_name));

			--log ("current dir " & current_directory, log_threshold + 1);
			
			log ("looking for module files ...", log_threshold + 1);
			log_indentation_up;
			start_search (module_file_search, current_directory, module_file_name_extension_asterisk, module_file_filter);
			if more_entries (module_file_search) then
				search (current_directory, module_file_name_extension_asterisk, module_file_filter, read_module_file'access);
			else
				log (message_warning & "No modules found !"); -- CS: write implications !
			end if;
			end_search (module_file_search);
			log_indentation_down;

			
			log ("looking for rig configuration files ...", log_threshold + 1);
			log_indentation_up;
			start_search (conf_file_search, current_directory, rig_configuration_file_extension_asterisk, conf_file_filter);
			if more_entries (conf_file_search) then
				search (current_directory, rig_configuration_file_extension_asterisk, conf_file_filter, read_conf_file'access);
			else
				log (message_warning & "No rig configuration files found !"); -- CS: write implications !
			end if;
			end_search (conf_file_search);
			log_indentation_down;
			
		else -- project directory does not exist
			log_indentation_reset;
			log (message_error & "Native project " & to_string (project_name) 
				 & " does not exist !", console => true);
			--log ("Example to open the native project by specifying the project directory:", console => true);			log ("Example to open the native project by specifying the project directory:", console => true);
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
		
		function parent return item is
		begin
			return s (top - 1);
		end parent;
		
	end stack_lifo;

end et_project;
	
-- Soli Deo Gloria
