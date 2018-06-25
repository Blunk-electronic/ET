------------------------------------------------------------------------------
--                                                                          --
--                           SYSTEM ET PCB                                  --
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
with ada.characters.handling;	use ada.characters.handling;

with ada.strings;				use ada.strings;
with ada.strings.maps;			use ada.strings.maps;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.strings.bounded; 		use ada.strings.bounded;
with ada.containers; 			use ada.containers;

with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_doubly_linked_lists;
with ada.containers.ordered_maps;
with ada.containers.indefinite_ordered_maps;
with ada.containers.ordered_sets;

with ada.exceptions;

with et_coordinates;
with et_libraries;
with et_string_processing;		use et_string_processing;

package body et_pcb is


	function to_string (directory_name : in type_directory_name.bounded_string) return string is
	-- Converts a directory name to a string.
	begin
		return type_directory_name.to_string (directory_name);
	end to_string;

	function to_string (signal_layer : in type_signal_layer) return string is
	begin
		--return trim (type_signal_layer'image (signal_layer), left);
		return type_signal_layer'image (signal_layer);
	end to_string;
	
	procedure validate_text_size (size : in type_distance) is
	-- Checks whether given text size is in range of type_text_size.
	begin
		if size not in type_text_size then
			log_indentation_reset;
			log (message_error & "text size invalid ! Allowed range is" 
				 & to_string (type_text_size'first) & " .."
				 & to_string (type_text_size'last),
				 console => true);
			raise constraint_error;
		end if;
	end validate_text_size;

	procedure validate_general_line_width (width : in type_distance) is
	-- Checks whether given line width is in range of type_general_line_width
	begin
		if width not in type_general_line_width then
			log_indentation_reset;
			log (message_error & "line width invalid ! Allowed range is" 
				 & to_string (type_general_line_width'first) & " .."
				 & to_string (type_general_line_width'last),
				 console => true);
			raise constraint_error;
		end if;
	end validate_general_line_width;

	procedure validate_text_line_width (width : in type_distance) is
	-- Checks whether given line width is in range of type_text_line_width
	begin
		if width not in type_text_line_width then
			log_indentation_reset;
			log (message_error & "line width invalid ! Allowed range is" 
				 & to_string (type_text_line_width'first) & " .."
				 & to_string (type_text_line_width'last),
				 console => true);
			raise constraint_error;
		end if;
	end validate_text_line_width;

	procedure validate_drill_size (drill : in type_distance) is
	-- Checks whether given drill size is in range of type_drill_size
	begin
		if drill not in type_drill_size then
			log_indentation_reset;
			log (message_error & "drill size invalid ! Allowed range is" 
				 & to_string (type_drill_size'first) & " .."
				 & to_string (type_drill_size'last),
				 console => true);
			raise constraint_error;
		end if;
	end validate_drill_size;

	procedure validate_pad_size (size : in type_distance) is
	-- Checks whether given pad size is in range of type_pad_size
	begin
		if size not in type_pad_size then
			log_indentation_reset;
			log (message_error & "pad size invalid ! Allowed range is" 
				 & to_string (type_pad_size'first) & " .."
				 & to_string (type_pad_size'last),
				 console => true);
			raise constraint_error;
		end if;
	end validate_pad_size;

	function to_string (drill : in type_drill) return string is
	-- returns the properties of the given drill as string.
	begin
		return (to_string (drill.position) & " drill_diameter" & to_string (drill.diameter));
	end to_string;
	
	procedure validate_signal_clearance (signal_clearance : in type_distance) is
	-- Checks whether the given signal clearance is in range of type_signal_clearance.
	begin
		if signal_clearance not in type_signal_clearance then
			log_indentation_reset;
			log (message_error & "signal clearance invalid ! Allowed range is" 
				 & to_string (type_signal_clearance'first) & " .."
				 & to_string (type_signal_clearance'last),
				 console => true);
			raise constraint_error;
		end if;
	end validate_signal_clearance;

	procedure validate_signal_width (signal_width : in type_distance) is
	-- Checks whether the given signal width is in range of type_signal_width.
	begin
		if signal_width not in type_signal_width then
			log_indentation_reset;
			log (message_error & "signal width invalid ! Allowed range is" 
				 & to_string (type_signal_width'first) & " .."
				 & to_string (type_signal_width'last),
				 console => true);
			raise constraint_error;
		end if;
	end validate_signal_width;

	procedure validate_restring_width (restring_width : in type_distance) is
	-- Checks whether the given restring width is in range of type_restring_width.	
	begin
		if restring_width not in type_restring_width then
			log_indentation_reset;
			log (message_error & "restring width invalid ! Allowed range is" 
				 & to_string (type_restring_width'first) & " .."
				 & to_string (type_restring_width'last),
				 console => true);
			raise constraint_error;
		end if;
	end validate_restring_width;


	
	-- VIAS
	function to_micro_vias_allowed (allowed : in string) return type_micro_vias_allowed is begin
		return type_micro_vias_allowed'value (allowed);
	end to_micro_vias_allowed;
	
	function to_string (allowed : in type_micro_vias_allowed) return string is begin
		return " micro vias allowed " & type_micro_vias_allowed'image (allowed);
	end to_string;


	-- NET CLASSES
	function to_string (net_class_name : in type_net_class_name.bounded_string) return string is
	begin
		return type_net_class_name.to_string (net_class_name);
	end to_string;

	
	
	function text_properties (text : in type_text) return string is
	-- Returns the properties of the given text in a long single string.
		use et_coordinates;
	begin
		return to_string (text.position) & latin_1.space
			& "size (x" & axis_separator & "y)" 
			& to_string (text.size_x) & latin_1.space & axis_separator & to_string (text.size_y)
			& " line width" & to_string (text.width)
			& to_string (text.angle, preamble => true)
			& et_libraries.to_string (text.alignment)
			& " hidden " & boolean'image (text.hidden)
			;
	end text_properties;
	
	function to_string (text_meaning : in type_text_meaning_package) return string is
	begin
		return type_text_meaning_package'image (text_meaning);
	end to_string;
	
	function to_directory (directory_name : in string) return type_directory_name.bounded_string is
	-- Converts a string to a type_directory_name.
	begin
		return type_directory_name.to_bounded_string (directory_name);
	end to_directory;

	procedure log_plated_millings (
		millings 		: in type_package_pcb_contour_plated;
		log_threshold	: in et_string_processing.type_log_level)
		is
		use type_pcb_contour_lines;
		use type_pcb_contour_arcs;
		use type_pcb_contour_circles;
		line_cursor 	: type_pcb_contour_lines.cursor;
		arc_cursor		: type_pcb_contour_arcs.cursor;
		circle_cursor	: type_pcb_contour_circles.cursor;
	begin
		if not is_empty (millings.lines) then
			line_cursor := millings.lines.first;
			while line_cursor /= type_pcb_contour_lines.no_element loop
				line_pcb_contour_properties (line_cursor, log_threshold);
				next (line_cursor);
			end loop;
		end if;
	end log_plated_millings;
	
		
	
	function to_string (appearance : in type_package_appearance) return string is
	begin
		return type_package_appearance'image (appearance);
	end to_string;
	
	function to_string (technology : in type_assembly_technology) return string is
	begin
		return latin_1.space & type_assembly_technology'image (technology);
	end to_string;

	function to_string (shape : in type_terminal_shape_tht) return string is
	begin
		return latin_1.space & type_terminal_shape_tht'image (shape);
	end to_string;

	function to_string (shape : in type_terminal_shape_smt) return string is
	begin
		return latin_1.space & type_terminal_shape_smt'image (shape);
	end to_string;

	function to_string (solder_paste : in type_terminal_solder_paste) return string is
	begin
		return latin_1.space & type_terminal_solder_paste'image (solder_paste);
	end to_string;

	function to_string (stop_mask : in type_terminal_stop_mask) return string is
	begin
		return latin_1.space & type_terminal_stop_mask'image (stop_mask);
	end to_string;

	function no_contour return type_package_contours is
	-- Returns an empty package contour.		
	begin
		return (
			lines 	=> type_package_contour_lines.empty_list,
			arcs 	=> type_package_contour_arcs.empty_list,
			circles	=> type_package_contour_circles.empty_list);
	end no_contour;

	function to_string (priority_level : in type_polygon_priority) return string is
	begin
		return type_polygon_priority'image (priority_level);
	end to_string;
	
	function to_polygon_priority (priority_level : in string) return type_polygon_priority is
	begin
		return type_polygon_priority'value (priority_level);
	end to_polygon_priority;
	
	function to_corner_easing (easing : in string) return type_corner_easing is
		easing_out : type_corner_easing;
	begin
		if to_lower (easing) = to_lower (type_corner_easing'image (NONE)) then
			easing_out := NONE;
		elsif to_lower (easing) = to_lower (type_corner_easing'image (CHAMFER)) then
			easing_out := CHAMFER;
		elsif to_lower (easing) = to_lower (type_corner_easing'image (FILLET)) then
			easing_out := FILLET;
		else
			log_indentation_reset;
			log (message_error & "type of easing '" & easing & "' invalid !", console => true);
			raise constraint_error;
		end if;

		return easing_out;
	end to_corner_easing;

	function to_string (easing : in type_corner_easing) return string is
	begin
		return latin_1.space & type_corner_easing'image (easing);
	end to_string;

	function to_string (polygon_pad_connection : in type_polygon_pad_connection) return string is
	begin
		return latin_1.space & type_polygon_pad_connection'image (polygon_pad_connection);
	end to_string;

	function to_string (polygon_pad_technology : in type_polygon_pad_technology) return string is
	begin
		return latin_1.space & type_polygon_pad_technology'image (polygon_pad_technology);
	end to_string;

	
	function to_string (line : in type_line) return string is
	-- Returns the start and end point of the given line as string.
	begin
		return "start" & to_string (line.start_point) 
			& " end" & to_string (line.end_point);
	end to_string;

	function to_string (arc : in type_arc) return string is
	-- Returns the start, end point and angle of the given arc as string.
	begin
		return "center" & to_string (arc.center) 
			& " start" & to_string (arc.start_point) 
			& " end" & to_string (arc.end_point);
	end to_string;

	function to_string (circle : in type_circle) return string is
	-- Returns the center and radius of the given circle as string.
	begin
		return "center" & to_string (circle.center) 
			& " radius" & to_string (circle.radius);
	end to_string;

	
	function to_string (description : in type_package_description.bounded_string) return string is
	begin
		return "description '" & type_package_description.to_string (description) & "'";
	end to_string;

	function to_package_description (description : in string) return type_package_description.bounded_string is
	begin
		return type_package_description.to_bounded_string (description);
	end to_package_description;

	
	function to_string (tags : in type_package_tags.bounded_string) return string is
	begin
		return "tags '" & type_package_tags.to_string (tags) & "'";
	end to_string;

	function to_package_tags (tags : in string) return type_package_tags.bounded_string is
	begin
		return type_package_tags.to_bounded_string (tags);
	end to_package_tags;

	function package_position (position : in type_package_position) return string is
	-- Returns the coordinates of a package (in a board) as string.
	begin
		return (" position" & to_string (position)
			& " angle" & to_string (get_angle (position))
			& " face" & to_string (get_face (position)));
	end package_position;

	
	
-- PROPERTIES OF OBJECTS IN COPPER (NON ELECTRIC !!)
	procedure line_copper_properties (
	-- Logs the properties of the given line of copper screen
		face			: in type_face;
		cursor			: in type_copper_lines.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is
		use type_copper_lines;
		line : type_copper_line;
	begin
		line := element (cursor);
		log ("copper line face" & to_string (face) & latin_1.space 
			 & to_string (type_line (line))
			 & " width" & to_string (line.width), log_threshold);
	end line_copper_properties;

	procedure arc_copper_properties (
	-- Logs the properties of the given arc of copper screen
		face			: in type_face;
		cursor			: in type_copper_arcs.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is
		use type_copper_arcs;
		arc : type_copper_arc;
	begin
		arc := element (cursor);
		log ("copper arc face" & to_string (face) & latin_1.space 
			 & to_string (type_arc (arc))
			 & " width" & to_string (arc.width), log_threshold);
	end arc_copper_properties;
	
	procedure circle_copper_properties (
	-- Logs the properties of the given circle of copper screen
		face			: in type_face;
		cursor			: in type_copper_circles.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is
		use type_copper_circles;
		circle : type_copper_circle;
	begin
		circle := element (cursor);
		log ("copper circle face" & to_string (face) & latin_1.space 
			 & to_string (type_circle (circle))
			 & " width" & to_string (circle.width), log_threshold);
	end circle_copper_properties;

	procedure text_copper_properties (
	-- Logs the properties of the given text of copper
		cursor			: in type_texts_with_content_pcb.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is
		use et_libraries.type_text_content;
		use type_texts_with_content_pcb;
		text : type_text_with_content_pcb;
	begin
		text := element (cursor);
		log ("copper text signal layer" & to_string (text.layer) & latin_1.space
			& "content '" & to_string (text.content) & "'", log_threshold
			);

		log_indentation_up;
		log (text_properties (type_text (text)), log_threshold + 1);
		log_indentation_down;
	end text_copper_properties;

	


-- PROPERTIES OF ELECTRIC OBJECTS IN SIGNAL LAYERS
	procedure route_line_properties (
	-- Logs the properties of the given line of a route
		cursor			: in type_copper_lines_pcb.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is
		use type_copper_lines_pcb;
		line : type_copper_line_pcb;
	begin
		line := element (cursor);
		log ("segment " & et_pcb.to_string (type_line (line)) &
			 " width" & to_string (line.width) &
			 " layer" & to_string (line.layer)
			 -- CS locked
			 , log_threshold);
	end route_line_properties;
	
	procedure route_via_properties (
	-- Logs the properties of the given via of a route
		cursor			: in type_vias.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is
		use type_vias;
		via : type_via;
	begin
		via := element (cursor);
		log ("via" & et_pcb.to_string (type_drill (via)) &
			 " restring_outer" & to_string (via.restring_outer) & -- outer layers
			 " restring_inner" & to_string (via.restring_inner) & -- inner layers
			 " layer_start" & to_string (via.layer_start) &
			 " layer_end" & to_string (via.layer_end)
			 -- CS locked
			 , log_threshold);
	end route_via_properties;

	procedure route_polygon_properties (
	-- Logs the properties of the given polygon of a route
		cursor			: in type_copper_polygons_pcb.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is
		use type_copper_polygons_pcb;
	begin
		-- general stuff
		log ("polygon in signal layer" & to_string (element (cursor).layer) &
			 " minimum_width" & to_string (element (cursor).width_min) &
			 " pad_connection" & to_string (element (cursor).pad_connection) &
			 " priority_level" & to_string (element (cursor).priority_level) &
			 " isolation_gap" & to_string (element (cursor).isolation_gap) &
			 " corner_easing" & to_string (element (cursor).corner_easing) &
			 " easing_radius" & to_string (element (cursor).easing_radius),
			 log_threshold);

		log_indentation_up;
		log ("corner_points : todo ", log_threshold);
		
		-- type depended stuff
		case element (cursor).pad_connection is
			when THERMAL =>
				log ("connects with" & to_string (element (cursor).thermal_technology) &
					" thermal width" & to_string (element (cursor).thermal_width) &
					" thermal gap" & to_string (element (cursor).thermal_gap),
					log_threshold);

			when SOLID =>
				log ("connects with" & to_string (element (cursor).solid_technology),
					log_threshold);
				
			when NONE =>
				null;
		end case;

		log_indentation_down;
	end route_polygon_properties;

-- PROPERTIES OF OBJECTS IN SILK SCREEN
	procedure line_silk_screen_properties (
	-- Logs the properties of the given line of silk screen
		face			: in type_face;
		cursor			: in type_silk_lines.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is
		use type_silk_lines;
		line : type_silk_line;
	begin
		line := element (cursor);
		log ("silk screen line face" & to_string (face) & latin_1.space 
			 & to_string (type_line (line))
			 & " width" & to_string (line.width), log_threshold);
	end line_silk_screen_properties;

	procedure arc_silk_screen_properties (
	-- Logs the properties of the given arc of silk screen
		face			: in type_face;
		cursor			: in type_silk_arcs.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is
		use type_silk_arcs;
		arc : type_silk_arc;
	begin
		arc := element (cursor);
		log ("silk screen arc face" & to_string (face) & latin_1.space 
			 & to_string (type_arc (arc))
			 & " width" & to_string (arc.width), log_threshold);
	end arc_silk_screen_properties;
	
	procedure circle_silk_screen_properties (
	-- Logs the properties of the given circle of silk screen
		face			: in type_face;
		cursor			: in type_silk_circles.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is
		use type_silk_circles;
		circle : type_silk_circle;
	begin
		circle := element (cursor);
		log ("silk screen circle face" & to_string (face) & latin_1.space 
			 & to_string (type_circle (circle))
			 & " width" & to_string (circle.width), log_threshold);
	end circle_silk_screen_properties;

	procedure placeholder_silk_screen_properties (
	-- Logs the properties of the given silk screen placeholder
		face			: in type_face;
		cursor			: in type_text_placeholders_package.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is
		use type_text_placeholders_package;
		placeholder : type_text_placeholder_package;
	begin
		placeholder := element (cursor);
		log ("silk screen placeholder face" & to_string (face)
			 & " for " & to_string (placeholder.meaning), log_threshold);
		
		log_indentation_up;
		log (text_properties (type_text (placeholder)), log_threshold + 1);
		log_indentation_down;
	end placeholder_silk_screen_properties;
	
	procedure text_silk_screen_properties (
	-- Logs the properties of the given silk screen text
		face			: in type_face;
		cursor			: in type_texts_with_content.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is
		use et_libraries.type_text_content;
		use type_texts_with_content;
		text : type_text_with_content;
	begin
		text := element (cursor);
		log ("silk screen text face" & to_string (face) & latin_1.space
			 & "content '" & to_string (text.content) & "'", log_threshold);

		log_indentation_up;
		log (text_properties (type_text (text)), log_threshold + 1);
		log_indentation_down;
	end text_silk_screen_properties;


	
-- PROPERTIES OF OBJECTS IN ASSEMBLY DOCUMENTATION
	procedure line_assy_doc_properties (
	-- Logs the properties of the given line of assembly documentation
		face			: in type_face;
		cursor			: in type_doc_lines.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is
		use type_doc_lines;
		line : type_doc_line;
	begin
		line := element (cursor);
		log ("assembly doc line face" & to_string (face) & latin_1.space
			 & to_string (type_line (line))
			 & " width" & to_string (line.width), log_threshold);
	end line_assy_doc_properties;

	procedure arc_assy_doc_properties (
	-- Logs the properties of the given arc of assembly documentation
		face			: in type_face;
		cursor			: in type_doc_arcs.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is
		use type_doc_arcs;
		arc : type_doc_arc;
	begin
		arc := element (cursor);
		log ("assembly doc arc face" & to_string (face) & latin_1.space 
			 & to_string (type_arc (arc))
			 & " width" & to_string (arc.width), log_threshold);
	end arc_assy_doc_properties;

	procedure circle_assy_doc_properties (
	-- Logs the properties of the given circle of assembly documentation
		face			: in type_face;
		cursor			: in type_doc_circles.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is
		use type_doc_circles;
		circle : type_doc_circle;
	begin
		circle := element (cursor);
		log ("assembly doc circle face" & to_string (face) & latin_1.space 
			 & to_string (type_circle (circle))
			 & " width" & to_string (circle.width), log_threshold);
	end circle_assy_doc_properties;

	procedure placeholder_assy_doc_properties (
	-- Logs the properties of the given assembly documentation placeholder
		face			: in type_face;
		cursor			: in type_text_placeholders_package.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is
		use type_text_placeholders_package;
		placeholder : type_text_placeholder_package;
	begin
		placeholder := element (cursor);
		log ("assembly doc placeholder face" & to_string (face)
			 & " for " & to_string (placeholder.meaning), log_threshold);

		log_indentation_up;
		log (text_properties (type_text (placeholder)), log_threshold + 1);
		log_indentation_down;
	end placeholder_assy_doc_properties;

	procedure text_assy_doc_properties (
	-- Logs the properties of the given assembly documentation text
		face			: in type_face;
		cursor			: in type_texts_with_content.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is
		use et_libraries.type_text_content;
		use type_texts_with_content;
		text : type_text_with_content;
	begin
		text := element (cursor);
		log ("assembly doc text face" & to_string (face) & latin_1.space
			 & "content '" & to_string (text.content) & "'", log_threshold);

		log_indentation_up;
		log (text_properties (type_text (text)), log_threshold + 1);
		log_indentation_down;
	end text_assy_doc_properties;


	
	
-- PROPERTIES OF OBJECTS IN KEEPOUT
	procedure line_keepout_properties (
	-- Logs the properties of the given line of keepout
		face			: in type_face;
		cursor			: in type_keepout_lines.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is
		use type_keepout_lines;
		line : type_keepout_line;
	begin
		line := element (cursor);
		log ("keepout (courtyard) line face" & to_string (face) & latin_1.space
			 & to_string (type_line (line)), log_threshold);
	end line_keepout_properties;

	procedure arc_keepout_properties (
	-- Logs the properties of the given arc of keepout
		face			: in type_face;
		cursor			: in type_keepout_arcs.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is
		use type_keepout_arcs;
		arc : type_keepout_arc;
	begin
		arc := element (cursor);
		log ("keepout (courtyard) arc face" & to_string (face) & latin_1.space 
			 & to_string (type_arc (arc)), log_threshold);
	end arc_keepout_properties;

	procedure circle_keepout_properties (
	-- Logs the properties of the given circle of keepout
		face			: in type_face;
		cursor			: in type_keepout_circles.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is
		use type_keepout_circles;
		circle : type_keepout_circle;
	begin
		circle := element (cursor);
		log ("keepout (courtyard) circle face" & to_string (face) & latin_1.space 
			 & to_string (type_circle (circle)), log_threshold);
	end circle_keepout_properties;


-- PROPERTIES OF OBJECTS IN STOP MASK
	procedure arc_stop_mask_properties (
	-- Logs the properties of the given arc of stop mask
		face			: in type_face;
		cursor			: in type_stop_arcs.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is
		use type_stop_arcs;
		arc : type_stop_arc;
	begin
		arc := element (cursor);
		log ("stop mask arc face" & to_string (face) & latin_1.space 
			 & to_string (type_arc (arc))
			 & " width" & to_string (arc.width),
			 log_threshold);
	end arc_stop_mask_properties;

	procedure circle_stop_mask_properties (
	-- Logs the properties of the given circle of stop mask
		face			: in type_face;
		cursor			: in type_stop_circles.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is
		use type_stop_circles;
		circle : type_stop_circle;
	begin
		circle := element (cursor);
		log ("stop mask circle face" & to_string (face) & latin_1.space 
			 & to_string (type_circle (circle))
			 & " width" & to_string (circle.width),
			 log_threshold);
	end circle_stop_mask_properties;

	procedure line_stop_mask_properties (
	-- Logs the properties of the given line of stop mask
		face			: in type_face;
		cursor			: in type_stop_lines.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is
		use type_stop_lines;
		line : type_stop_line;
	begin
		line := element (cursor);
		log ("stop mask line face" & to_string (face) & latin_1.space
			 & to_string (type_line (line))
			 & " width" & to_string (line.width),
			 log_threshold);
	end line_stop_mask_properties;

	procedure text_stop_mask_properties (
	-- Logs the properties of the given stop mask text
		face			: in type_face;
		cursor			: in type_texts_with_content.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is
		use et_libraries.type_text_content;
		use type_texts_with_content;
		text : type_text_with_content;
	begin
		text := element (cursor);
		log ("stop mask text face" & to_string (face) & latin_1.space
			 & "content '" & to_string (text.content) & "'", log_threshold);

		log_indentation_up;
		log (text_properties (type_text (text)), log_threshold + 1);
		log_indentation_down;
	end text_stop_mask_properties;


-- PROPERTIES OF OBJECTS IN SOLDER PASTE / STENCIL
	procedure arc_stencil_properties (
	-- Logs the properties of the given arc of stencil
		face			: in type_face;
		cursor			: in type_stencil_arcs.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is
		use type_stencil_arcs;
		arc : type_stencil_arc;
	begin
		arc := element (cursor);
		log ("solder paste (stencil) arc face" & to_string (face) & latin_1.space 
			 & to_string (type_arc (arc))
			 & " width" & to_string (arc.width),
			 log_threshold);
	end arc_stencil_properties;

	procedure circle_stencil_properties (
	-- Logs the properties of the given circle of stencil
		face			: in type_face;
		cursor			: in type_stencil_circles.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is
		use type_stencil_circles;
		circle : type_stencil_circle;
	begin
		circle := element (cursor);
		log ("solder paste (stencil) circle face" & to_string (face) & latin_1.space 
			 & to_string (type_circle (circle))
 			 & " width" & to_string (circle.width),
			 log_threshold);
	end circle_stencil_properties;

	procedure line_stencil_properties (
	-- Logs the properties of the given line of stencil
		face			: in type_face;
		cursor			: in type_stencil_lines.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is
		use type_stencil_lines;
		line : type_stencil_line;
	begin
		line := element (cursor);
		log ("solder paste (stencil) line face" & to_string (face) & latin_1.space
			 & to_string (type_line (line))
			 & " width" & to_string (line.width),
			 log_threshold);
	end line_stencil_properties;
	
	
	
-- PROPERTIES OF OBJECTS IN ROUTE RESTRICT
	procedure line_route_restrict_properties (
	-- Logs the properties of the given line of route restrict
		face			: in type_face;
		cursor			: in type_route_restrict_lines.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is
		use type_route_restrict_lines;
		line : type_route_restrict_line;
	begin
		line := element (cursor);
		log ("route restrict line face" & to_string (face) & latin_1.space
			 & to_string (type_line (line)), log_threshold);
	end line_route_restrict_properties;

	procedure arc_route_restrict_properties (
	-- Logs the properties of the given arc of route restrict
		face			: in type_face;
		cursor			: in type_route_restrict_arcs.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is
		use type_route_restrict_arcs;
		arc : type_route_restrict_arc;
	begin
		arc := element (cursor);
		log ("route restrict arc face" & to_string (face) & latin_1.space 
			 & to_string (type_arc (arc)), log_threshold);
	end arc_route_restrict_properties;



-- PROPERTIES OF OBJECTS IN VIA RESTRICT	
	procedure line_via_restrict_properties (
	-- Logs the properties of the given line of via restrict
		face			: in type_face;
		cursor			: in type_via_restrict_lines.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is
		use type_via_restrict_lines;
		line : type_via_restrict_line;
	begin
		line := element (cursor);
		log ("via restrict line face" & to_string (face) & latin_1.space
			 & to_string (type_line (line)), log_threshold);
	end line_via_restrict_properties;

	procedure arc_via_restrict_properties (
	-- Logs the properties of the given arc of via restrict
		face			: in type_face;
		cursor			: in type_via_restrict_arcs.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is
		use type_via_restrict_arcs;
		arc : type_via_restrict_arc;
	begin
		arc := element (cursor);
		log ("via restrict arc face" & to_string (face) & latin_1.space 
			 & to_string (type_arc (arc)), log_threshold);
	end arc_via_restrict_properties;


-- PROPERTIES OF OBJECTS IN BOARD CONTOUR / OUTLINE / EDGE CUTS
	procedure line_pcb_contour_properties (
	-- Logs the properties of the given line of pcb contour
		cursor			: in type_pcb_contour_lines.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is
		use type_pcb_contour_lines;
		line : type_pcb_contour_line;
	begin
		line := element (cursor);
		log ("PCB contour (edge cuts / outline) line face" & latin_1.space
			 & to_string (type_line (line)), log_threshold);
	end line_pcb_contour_properties;

	procedure arc_pcb_contour_properties (
	-- Logs the properties of the given arc of pcb contour
		cursor			: in type_pcb_contour_arcs.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is
		use type_pcb_contour_arcs;
		arc : type_pcb_contour_arc;
	begin
		arc := element (cursor);
		log ("PCB contour (edge cuts / outline) arc" & latin_1.space 
			 & to_string (type_arc (arc)), log_threshold);
	end arc_pcb_contour_properties;

	procedure circle_pcb_contour_properties (
	-- Logs the properties of the given circle of pcb contour
		cursor			: in type_pcb_contour_circles.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is
		use type_pcb_contour_circles;
		circle : type_pcb_contour_circle;
	begin
		circle := element (cursor);
		log ("PCB contour (edge cuts / outline) circle face" & latin_1.space 
			 & to_string (type_circle (circle)), log_threshold);
	end circle_pcb_contour_properties;


	
	
	
	

	procedure terminal_properties (
	-- Logs the properties of the given terminal.
		terminal		: in et_pcb.type_terminal;
		name			: in et_libraries.type_terminal_name.bounded_string;
		log_threshold 	: in et_string_processing.type_log_level) is
		use et_pcb_coordinates;
		use et_libraries;
		log_threshold_1 : type_log_level := log_threshold + 1;
	begin
		log ("terminal name " & to_string (name)
			& " technology" & to_string (terminal.technology)
			& to_string (type_point_3d (terminal.position))
			& to_string (angle => get_angle (terminal.position), preamble => true),
			log_threshold);

		log_indentation_up;
		case terminal.technology is
			when THT => 
				log ("shape" & to_string (terminal.shape_tht), log_threshold_1);
				log ("copper width of inner layers" & to_string (terminal.width_inner_layers), log_threshold_1);
				case terminal.shape is
					when NON_CIRCULAR =>
						log ("size x" & to_string (terminal.size_tht_x), log_threshold_1);
						log ("size y" & to_string (terminal.size_tht_y), log_threshold_1);
						case terminal.tht_hole is
							when DRILLED =>
								log ("drill" & to_string (terminal.drill_size_dri), log_threshold_1); 
							when MILLED =>
								if log_level >= log_threshold_1 then
									log ("plated milling contour ");
									log_indentation_up;
									log_plated_millings (terminal.millings, log_threshold_1);
									log_indentation_down;
								end if;
						end case;
						
					when CIRCULAR =>
						log ("drill" & to_string (terminal.drill_size_cir), log_threshold_1); 
				end case;
				
			when SMT => 
				log ("shape" & to_string (terminal.shape_smt), log_threshold_1);

				case terminal.shape is
					when NON_CIRCULAR =>
						log ("size x" & to_string (terminal.size_smt_x), log_threshold_1);
						log ("size y" & to_string (terminal.size_smt_y), log_threshold_1);
					when CIRCULAR =>
						null; -- CS 
				end case;
				
				log ("face" & to_string (terminal.face), log_threshold_1);
				log ("stop mask" & to_string (terminal.stop_mask), log_threshold_1);
				log ("solder paste" & to_string (terminal.solder_paste), log_threshold_1);
		end case;

		log_indentation_down;
	end terminal_properties;


	
	function terminal_count (
	-- Returns the number of terminals of the given package in the given library.
		library_name		: in et_libraries.type_full_library_name.bounded_string;
		package_name 		: in et_libraries.type_component_package_name.bounded_string)
		return et_libraries.type_terminal_count is

		use et_libraries;
		use type_libraries;
		
		terminals : et_libraries.type_terminal_count; -- to be returned
		library_cursor : type_libraries.cursor; -- points to the library

		procedure locate_package (
			library_name	: in type_full_library_name.bounded_string;
			packages		: in type_packages_library.map) is
			use type_terminals;
			package_cursor : type_packages_library.cursor;
		begin
			-- locate the package
			package_cursor := packages.find (package_name);

			-- get number of terminals
			terminals := type_terminal_count (length (element (package_cursor).terminals));
		end locate_package;
		
	begin -- terminal_count
		-- locate the library
		library_cursor := type_libraries.find (package_libraries, library_name);

		if library_cursor = type_libraries.no_element then
			log_indentation_reset;
			log (message_error & to_string (library_name) & " not found !", console => true);
			raise constraint_error;
		else
			-- query packages in library
			type_libraries.query_element (library_cursor, locate_package'access);
		end if;
		
		return terminals;

		exception
			when event:
				others =>
					log_indentation_reset;
					log (ada.exceptions.exception_message (event), console => true);
					raise;

	end terminal_count;
	
	function terminal_port_map_fits (
	-- Used when terminal_port_maps are to be used for packages.
	-- The given package is specified by the library name and package name.
	-- Returns true if the terminal_port_map fits on the given package.
		library_name		: in et_libraries.type_full_library_name.bounded_string;		-- ../lbr/bel_ic.pretty
		package_name 		: in et_libraries.type_component_package_name.bounded_string;	-- S_SO14
		terminal_port_map	: in et_libraries.type_terminal_port_map.map) 
		return boolean is

		use et_libraries;
		use type_libraries;
		library_cursor : type_libraries.cursor;

		procedure validate_terminals (package_terminals : in type_terminals.map) is
		-- Test if the terminals of the terminal_port_map are also in the given package.
		-- Raises constraint_error if a terminal could not be found in the package.
			use type_terminals; -- the terminals of the package
			use et_libraries.type_terminal_port_map;
		
			-- This cursor points to the terminal in the terminal_port_map
			terminal_cursor : et_libraries.type_terminal_port_map.cursor; 

			-- For temporarily storage of a terminal name:
			terminal_name_in_map : et_libraries.type_terminal_name.bounded_string;
		begin -- validate_terminals
			-- Loop in terminal_port_map. Test each terminal whether it occurs
			-- in the package_terminals.
			terminal_cursor := terminal_port_map.first;
			while terminal_cursor /= type_terminal_port_map.no_element loop
				terminal_name_in_map := key (terminal_cursor);

				if package_terminals.find (terminal_name_in_map) = type_terminals.no_element then
					log_indentation_reset;
					log (message_error & "package " & to_string (packge => package_name)
						 & " does not have a terminal '" 
						 & to_string (terminal_name_in_map) & "' !", console => true);
					raise constraint_error;
				end if;
				
				next (terminal_cursor);
			end loop;
		end validate_terminals;
			
	
		procedure locate_package (
		-- Locates the package by package_name in the given package library.
			library_name	: in type_full_library_name.bounded_string;
			packages		: in type_packages_library.map) is
			package_cursor : type_packages_library.cursor;

			use type_terminals;
			use type_terminal_port_map;
			terminals : et_libraries.type_terminal_count;
		begin
			if is_empty (packages) then
				log_indentation_reset;
				log (message_error & "package library " & to_string (library_name)
					 & " is empty !", console => true);
				raise constraint_error;
			else
				-- locate the package
				package_cursor := packages.find (package_name);
				if package_cursor = type_packages_library.no_element then
					log_indentation_reset;
					log (message_error & "package " & to_string (packge => package_name)
						& " not found in library " & to_string (library_name)
						& " !", console => true);
					raise constraint_error;
				else
					-- load the total number of terminals the package provides
					terminals := type_terminal_count (length (element (package_cursor).terminals));

					-- If the package has less terminals than the given terminal_port_map abort:
					if terminals < type_terminal_count (length (terminal_port_map)) then
						log_indentation_reset;
						log (message_error & "package " & to_string (packge => package_name)
							& " as too little terminals !",
							console => true);
						raise constraint_error;
					else
						validate_terminals (element (package_cursor).terminals);
					end if;
					
				end if;

			end if;
			
		end locate_package;
		
	begin -- terminal_port_map_fits
		if not is_empty (package_libraries) then
			library_cursor := package_libraries.find (library_name);

			if library_cursor = type_libraries.no_element then
				log_indentation_reset;
				log (message_error & "package library " & to_string (library_name)
					 & " not found in " & to_string (lib_dir) 
					 & " !", console => true);
				raise constraint_error;
			else
				-- locate the given package (by package_name) in the given package library:
				query_element (
					position	=> library_cursor,
					process		=> locate_package'access);
			end if;
				
		else
			log_indentation_reset;
			log (message_error & "no package libraries available !", console => true);
			raise constraint_error;
		end if;

		return true;

		exception
			when event:
				others =>
					log_indentation_reset;
					log (ada.exceptions.exception_message (event), console => true);
					raise;

	end terminal_port_map_fits;

	
end et_pcb;

-- Soli Deo Gloria
