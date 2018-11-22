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
	
	
-- NATIVE PROJECT

-- 	procedure create_libraries_directory_components (
-- 	-- Creates a directory where component libraries will live.
-- 	-- An already existing directory will be overwritten.
-- 	-- Sets the global library directory name so that subsequent write and read operations
-- 	-- access the right directory.
-- 		project_path	: in type_et_project_path.bounded_string;
-- 		log_threshold	: in et_string_processing.type_log_level) is
-- 		use et_general;
-- 		use ada.directories;
-- 		use et_string_processing;
-- 		use type_project_name;
-- 		use type_et_project_path;
-- 
-- 		path_length : positive :=  project_path_max + directory_libraries'length + directory_libraries_components'length + 2; -- incl. directory separators
-- 		package type_path is new generic_bounded_length (path_length);
-- 		use type_path;
-- 		path : type_path.bounded_string;
-- 	begin -- create_libraries_directory_components
-- 		path := to_bounded_string (compose (to_string (project_path), directory_libraries));
-- 		path := to_bounded_string (compose (to_string (path), directory_libraries_components));
-- 		
-- -- 		log ("creating in " & current_directory & " a new " & et_general.system_name & " libraries directory " 
-- -- 			 & to_string (path) & " ...",
-- -- 			log_threshold);
-- 		log ("directory for project wide libraries '" & directory_libraries & "' ...", log_threshold);
-- 
-- 		log_indentation_up;
-- 		
-- 		-- delete previous libraries directory
-- 		if exists (to_string (path)) then
-- 			delete_tree (to_string (path));
-- 		end if;
-- 		
-- 		-- create the libraries directory
-- 		create_path (to_string (path));
-- 
-- 		-- set the global library directory name
-- 		log ("setting global library directory name ...", log_threshold + 1);
-- 		component_libraries_directory_name := type_libraries_directory.to_bounded_string (to_string (path));
-- 	
-- 		log (" global library directory name is now " 
-- 			 & type_libraries_directory.to_string (component_libraries_directory_name), log_threshold + 2);
-- 
-- 		log_indentation_down;
-- 		
-- 		exception when event:
-- 			others => 
-- 				log (ada.exceptions.exception_message (event), console => true);
-- 				raise;
-- 		
-- 	end create_libraries_directory_components;


	procedure create_project_directory (
	-- Creates given project directory in the given project_path.
	-- Already existing projects in given project_path are overwritten.
	-- Sets the global project file name so that subsequent write and read operations
	-- know the right project file.
	-- Leaves the project file (global project_file_handle) open (closes it on exception).
		project_name	: in type_project_name.bounded_string;
		project_path	: in type_et_project_path.bounded_string;
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
			
	begin
-- 		log ("creating in " & current_directory & " a new " & et_general.system_name & " project directory " 
-- 			 & to_string (path) & " ...",
-- 			log_threshold);
		log ("project name '" & to_string (project_name) & "' ...", log_threshold);

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

		-- set the global project_file_name
		log ("setting global project file name ...", log_threshold + 1);
		project_file_name := type_project_file_name.to_bounded_string (compose (
			containing_directory	=> to_string (path),
			name 					=> to_string (project_name),
			extension 				=> project_file_name_extension));
		
		log (" global project file name is now " & type_project_file_name.to_string (project_file_name), log_threshold + 2);

		-- create project file and write in it a header
		create (
			file => project_file_handle,
			mode => out_file, 
			name => type_project_file_name.to_string (project_file_name));

		put_line (project_file_handle, comment_mark & " " & system_name & " project file");
		put_line (project_file_handle, comment_mark & " " & date);
		put_line (project_file_handle, comment_mark & " project " & to_string (project_name));
		put_line (project_file_handle, comment_mark & " " & row_separator_double);
		new_line (project_file_handle);

		log_indentation_down;
		
		exception when event:
			others => 
				log (ada.exceptions.exception_message (event), console => true);
				close (project_file_handle);
				raise;
		
	end create_project_directory;

	
	procedure save_project (log_threshold : in et_string_processing.type_log_level) is
	-- Saves the schematic and layout data in project file (project_file_handle).
		use et_string_processing;
		use et_schematic;
		use et_schematic.type_rig;
		
		procedure write_project_footer is
		-- writes a nice footer in the project file and closes it.
		begin
			log ("closing project file ...", log_threshold + 1);
			put_line (comment_mark & " " & row_separator_double);
			put_line (comment_mark & " " & date);
			put_line (comment_mark & " project " & to_string (project_name) & " file end");
			new_line;
		end write_project_footer;

		tab_depth : natural := 0;

		tab : character renames tabulator;
		space : character renames latin_1.space;
		
		procedure tab_depth_up is begin
			tab_depth := tab_depth + 1;
		end tab_depth_up;

		procedure tab_depth_down is begin
			tab_depth := tab_depth - 1;
		end tab_depth_down;

		type type_section_mark is (HEADER, FOOTER);
		
		procedure section_mark (section : in string; mark : in type_section_mark) is begin
			case mark is
				when HEADER =>
					--new_line;
					put_line (tab_depth * tabulator & section & space & section_begin);
					tab_depth_up;
				when FOOTER =>
					tab_depth_down;
					put_line (tab_depth * tabulator & section & space & section_end);
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
				put_line (tab_depth * tabulator & keyword & latin_1.space & parameters_wrapped);
			else
				case space is
					when true =>
						put_line (tab_depth * tabulator & keyword & latin_1.space & parameters);
					when false =>
						put_line (tab_depth * tabulator & keyword & parameters);
				end case;
			end if;
		end write;

		module_cursor : type_rig.cursor := rig.first;

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

		function rotation (angle : in et_coordinates.type_angle) return string is
		begin
			return type_angle'image (angle);
		end rotation;

		function rotation (pos : in et_pcb_coordinates.type_terminal_position'class) return string is
			use et_pcb_coordinates;
		begin
			return to_string (get_angle (pos));
		end rotation;
		
		function position (point : et_pcb_coordinates.type_point_2d'class) return string is
			use et_pcb_coordinates;
		begin
			return space & keyword_pos_x & to_string (get_axis (X, point)) 
				& space & keyword_pos_y & to_string (get_axis (Y, point));
		end position;

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

		procedure write_text_properties (text : in et_libraries.type_text_basic'class) is
			use et_coordinates;
		begin
			write (keyword => keyword_position, parameters => position (text.position));
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
		
		function face (point : et_pcb_coordinates.type_package_position) return string is
			use et_pcb_coordinates;
		begin
			return to_string (get_face (point));
		end face;
		
		procedure query_net_classes (module_name : in type_submodule_name.bounded_string; module : in type_module) is
			use et_pcb;
			use et_pcb.type_net_classes;
			class_cursor : et_pcb.type_net_classes.cursor := module.net_classes.first;

			use et_pcb_coordinates;
		begin
			log_indentation_up;
			section_mark (section_net_classes, HEADER);
			while class_cursor /= type_net_classes.no_element loop
				log ("net class " & to_string (key (class_cursor)), log_threshold + 1);
				section_mark (section_net_class, HEADER);

				write (keyword => keyword_name, parameters => to_string (key (class_cursor)));
				write (keyword => keyword_description, parameters => et_pcb.to_string (element (class_cursor).description), wrap => true);
				write (keyword => keyword_clearance, parameters => to_string (element (class_cursor).clearance));
				write (keyword => keyword_track_width_min, parameters => to_string (element (class_cursor).signal_width_min));
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

		procedure query_nets (module_name : in type_submodule_name.bounded_string; module : in type_module) is
			use et_schematic;
			use et_schematic.type_nets;
			net_cursor : et_schematic.type_nets.cursor := module.nets.first;

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
				write (keyword => keyword_class, parameters => to_string (element (net_cursor).class));
				write (keyword => keyword_scope, parameters => to_string (element (net_cursor).scope));

				query_element (net_cursor, query_strands'access);
				query_element (net_cursor, query_route'access);
				
				section_mark (section_net, FOOTER);
				next (net_cursor);
			end loop;
			section_mark (section_nets, FOOTER);
			
			log_indentation_down;
		end query_nets;

		procedure query_devices (module_name : in type_submodule_name.bounded_string; module : in type_module) is
			use et_schematic;
			use type_devices;
			device_cursor : et_schematic.type_devices.cursor := module.devices.first;

			procedure query_units (device_name : in et_libraries.type_component_reference; device : in et_schematic.type_device) is
				use et_schematic.type_units;
				unit_cursor : type_units.cursor := device.units.first;

				procedure write_placeholder (ph : in et_libraries.type_text_placeholder) is
				begin
					section_mark (section_placeholder, HEADER);
					write (keyword => keyword_meaning, parameters => et_libraries.to_string (ph.meaning));
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

		procedure query_frames (module_name : in type_submodule_name.bounded_string; module : in type_module) is
			-- CS: handle sheet description 
			use et_libraries;
			use type_frame_template_name;
		begin
			section_mark (section_drawing_frames, HEADER);
			section_mark (section_schematic, HEADER);			
			write (keyword => keyword_template, parameters => to_string (module.frame_template_schematic));
			section_mark (section_schematic, FOOTER);			

			section_mark (section_board, HEADER);			
			write (keyword => keyword_template, parameters => to_string (module.frame_template_board));
			section_mark (section_board, FOOTER);			
			section_mark (section_drawing_frames, FOOTER);
		end query_frames;

		procedure query_submodules (module_name : in type_submodule_name.bounded_string; module : in type_module) is
			use et_schematic;
			use type_submodules;
			submodule_cursor : type_submodules.cursor := module.submodules.first;
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
				--write (keyword => keyword_position_in_board, parameter => position (element (submodule_cursor).position_in_board
				section_mark (section_submodule, FOOTER);				
				next (submodule_cursor);
			end loop;
			section_mark (section_submodules, FOOTER);
		end query_submodules;
		
	begin -- save_project
		log ("saving project ...", log_threshold);
		set_output (project_file_handle);
		
		-- write content
		log_indentation_up;
		
		while module_cursor /= et_schematic.type_rig.no_element loop
			
			log ("module " & to_string (key (module_cursor)), log_threshold);
			section_mark (section_module, HEADER);

			-- generic module name
			write (keyword => keyword_generic_name, parameters => to_string (element (module_cursor).generic_name), space => true);

			-- module instance name
			write (keyword => keyword_instance_name, parameters => to_string (element (module_cursor).instance), space => true);

			-- net classes
			query_element (module_cursor, query_net_classes'access);

			-- nets
			query_element (module_cursor, query_nets'access);

			-- frames
			query_element (module_cursor, query_frames'access);
			
			-- notes
			
			-- submodules
			query_element (module_cursor, query_submodules'access);
			-- devices
			query_element (module_cursor, query_devices'access);
			
			-- board



			
			section_mark (section_module, FOOTER);
			
			new_line;
			put_line (comment_mark & row_separator_single);
			new_line;
			next (module_cursor);
		end loop;
		
		log_indentation_down;
		
		-- close native project file
		write_project_footer;

		set_output (standard_output);		
		close (project_file_handle);

		exception when event:
			others => 
				log (ada.exceptions.exception_message (event), console => true);
				close (project_file_handle);
				raise;

	end save_project;


	procedure open_project (log_threshold : in et_string_processing.type_log_level) is
	-- Opens and reads the schematic and layout data present in project file (project_file_handle).
		use et_string_processing;
		use ada.directories;

		line : et_string_processing.type_fields_of_line;

		procedure process_line is 
		begin
			put_line (standard_output, to_string (line));
			-- CS read content
		end process_line;
				
	begin
		log ("opening project '" & to_string (project_name) & "' ...", log_threshold, console => true);
		log_indentation_up;

		--log ("directory " & base_name (to_string (project_name)));
		
		project_file_name := type_project_file_name.to_bounded_string (compose (
			containing_directory	=> to_string (project_name),
			name 					=> base_name (to_string (project_name)),
			extension 				=> project_file_name_extension));

		log ("project file is " & type_project_file_name.to_string (project_file_name), log_threshold + 1);
		
		if exists (type_project_file_name.to_string (project_file_name)) then

			-- open project file
			open (
				file => project_file_handle,
				mode => in_file, 
				name => type_project_file_name.to_string (project_file_name));

			set_input (project_file_handle);

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

			
			set_input (standard_input);
			close (project_file_handle);

		else
			log_indentation_reset;
			log (message_error & "Native project file " & type_project_file_name.to_string (project_file_name) 
				 & " does not exist !", console => true);
			--log ("Example to open the native project by specifying the project directory:", console => true);			log ("Example to open the native project by specifying the project directory:", console => true);
			--log (system_name_cmd_line & "openetample to open the native project by specifying the project directory:", console => true);
			raise constraint_error;
		end if;
		log_indentation_down;
		
		exception when event:
			others => 
				log (ada.exceptions.exception_message (event), console => true);
				if is_open (project_file_handle) then
					close (project_file_handle);
				end if;
				raise;

	end open_project;
	
-- 	procedure write_component_libraries (log_threshold : in et_string_processing.type_log_level) is
-- 	-- Writes the ET native libraries in libraries_directory_name.
-- 	-- Creates sub-directories for library groups (like active, passive, misc, ...)
-- 	-- CS: currently only one group is supported. See et_libraries.library_group .
-- 	-- Each group is further-on composed of sub-directories for symbols, packages and devices.
-- 		use et_string_processing;
-- 		use ada.directories;
-- 		-- The group may be a path like "../../lbr" or "../passive". 
-- 		-- We are interested in the simple name like "lbr" or "passive".
-- 		lib_group_length : positive := simple_name (et_libraries.to_string (et_libraries.library_group))'length;
-- 
-- 		-- The path where the group is to be stored is composed of the libraries_directory_name and the group name.
-- 		path_length : positive := type_libraries_directory.length (component_libraries_directory_name) + lib_group_length + 1; -- incl. directory separator
-- 		package type_path is new generic_bounded_length (path_length);
-- 		use type_path;
-- 		path : type_path.bounded_string;
-- 
-- -- 		procedure w
-- -- 		device_file_handle : ada.text_io.file_type;
-- 	begin -- write_component_libraries
-- 		
-- 		-- set the path of the library group:
-- 		path := to_bounded_string (
-- 				  compose (
-- 					type_libraries_directory.to_string (component_libraries_directory_name), -- "components"
-- 					simple_name (et_libraries.to_string (et_libraries.library_group)) -- "passive"
-- 					)
-- 				);
-- 
-- 		-- create library group (CS or lots of groups in the future, see comments above)
-- 		create_path (to_string (path));
-- 
-- 		-- create sub-directories for symbols, packages and devices.
-- 		create_path (compose (to_string (path), directory_libraries_components_sym));
-- 		create_path (compose (to_string (path), directory_libraries_components_pac));
-- 		create_path (compose (to_string (path), directory_libraries_components_dev));
-- 		
-- 		log ("writing native libraries in " & to_string (path) & " ...", log_threshold);
-- 
-- 		
-- 		
-- 		exception when event:
-- 			others => 
-- 				log (ada.exceptions.exception_message (event), console => true);
-- 				raise;
-- 
-- 	end write_component_libraries;
	
end et_project;
	
-- Soli Deo Gloria
