------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                             PACKAGES                                     --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2021 Mario Blunk, Blunk electronic          --
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

with ada.strings;				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;

with ada.exceptions;

package body et_packages is

	function to_layer_category (cat : in string) return type_layer_category is begin
		return type_layer_category'value (layer_category_prefix & cat);
	end to_layer_category;

	function to_string (cat : in type_layer_category) return string is
		s : string := type_layer_category'image (cat);
	begin
		return s (layer_category_prefix'length + 1 .. s'last);
	end to_string;


	
	function to_string (packge : in pac_package_name.bounded_string) return string is
	-- Returns the given package name as string.
	-- CS: provide a parameter that turns the preamble on/off
	begin
		return pac_package_name.to_string (packge);
	end to_string;

	function to_package_name (package_name : in string) return pac_package_name.bounded_string is
	-- Converts a string to a pac_package_name.	
	begin
		return pac_package_name.to_bounded_string (package_name);
	end to_package_name;
	
	procedure check_package_name_length (packge : in string) is
	-- Tests if the given package is longer than allowed.
		use et_string_processing;
	begin
		if packge'length > package_name_length_max then
			log (WARNING, "package name too long. Max. length is" 
				 & positive'image (package_name_length_max) & " !");
		end if;
	end check_package_name_length;

	procedure check_package_name_characters (
		packge		: in pac_package_name.bounded_string;
		characters	: in character_set := package_name_characters)
	is
		use et_string_processing;
		use pac_package_name;
		invalid_character_position : natural := 0;
	begin
		invalid_character_position := index (
			source => packge,
			set => characters,
			test => outside);

		if invalid_character_position > 0 then
			log (WARNING, "package name " & enclose_in_quotes (to_string (packge))
				 & " has invalid character at position"
				 & natural'image (invalid_character_position));
		end if;
	end check_package_name_characters;




	
	function to_string (name : in pac_package_model_file_name.bounded_string) 
		return string is
	begin
		return pac_package_model_file_name.to_string (name);
	end;

	function to_file_name (name : in string) 
		return pac_package_model_file_name.bounded_string is
	begin
		return pac_package_model_file_name.to_bounded_string (name);
	end;


	


	procedure validate_general_line_width (width : in type_distance) is
	begin
		if width not in type_general_line_width then
			log (ERROR, "line width invalid ! Allowed range is" 
				 & to_string (type_general_line_width'first) & " .."
				 & to_string (type_general_line_width'last),
				 console => true);
			raise constraint_error;
		end if;
	end validate_general_line_width;

	procedure validate_pad_size (size : in type_distance) is
	begin
		if size not in type_pad_size then
			log (ERROR, "pad size invalid ! Allowed range is" 
				 & to_string (type_pad_size'first) & " .."
				 & to_string (type_pad_size'last),
				 console => true);
			raise constraint_error;
		end if;
	end validate_pad_size;
	
-- 	procedure validate_track_clearance (clearance : in et_pcb_coordinates.type_distance) is
-- 	-- Checks whether the given track clearance is in range of type_track_clearance.
-- 	begin
-- 		if clearance not in type_track_clearance then
-- 			log (ERROR, "track clearance invalid ! Allowed range is" 
-- 				 & to_string (type_track_clearance'first) & " .."
-- 				 & to_string (type_track_clearance'last),
-- 				 console => true);
-- 			raise constraint_error;
-- 		end if;
-- 	end validate_track_clearance;
-- 
-- 	procedure validate_track_width (track_width : in type_distance_positive) is
-- 	-- Checks whether the given width is in range of type_track_width.
-- 	begin
-- 		if track_width not in type_track_width then
-- 			log (ERROR, "track width invalid ! Allowed range is" 
-- 				 & to_string (type_track_width'first) & " .."
-- 				 & to_string (type_track_width'last),
-- 				 console => true);
-- 			raise constraint_error;
-- 		end if;
-- 	end validate_track_width;

-- 	procedure validate_restring_width (restring_width : in et_pcb_coordinates.type_distance) is
-- 	-- Checks whether the given restring width is in range of type_restring_width.	
-- 	begin
-- 		if restring_width not in type_restring_width then
-- 			log (ERROR, "restring width invalid ! Allowed range is" 
-- 				 & to_string (type_restring_width'first) & " .."
-- 				 & to_string (type_restring_width'last),
-- 				 console => true);
-- 			raise constraint_error;
-- 		end if;
-- 	end validate_restring_width;

	
	function text_properties (text : in type_text) return string is
	-- Returns the properties of the given text in a long single string.
	begin
		-- CS use text_properties in generic package text
		return to_string (text.position) & latin_1.space
			& "size" 
			& to_string (text.size)
			& " line width" & to_string (text.line_width)
			& " rotation" & to_string (rot (text.position))
			& et_text.to_string (text.alignment)
			-- CS & " hidden " & boolean'image (text.hidden)
			;
	end text_properties;
	
	function to_string (text_meaning : in type_text_meaning_package) return string is begin
		return to_lower (type_text_meaning_package'image (text_meaning));
	end;

	function to_text_meaning (text_meaning : in string) return type_text_meaning_package is begin
		return type_text_meaning_package'value (text_meaning);
	end;

	function to_string (layer : in type_placeholder_package_layer) return string is begin
		return to_lower (type_placeholder_package_layer'image (layer));
	end;

	function to_layer (layer : in string) return type_placeholder_package_layer is begin
		return type_placeholder_package_layer'value (layer);
	end;

	-- FILL STYLE
	function to_string (fill_style : in type_fill_style) return string is begin
		return to_lower (type_fill_style'image (fill_style));
	end;

	function to_fill_style (fill_style : in string) return type_fill_style is begin
		return type_fill_style'value (fill_style);
	end;

	
	-- EASING
	function to_easing_style (easing : in string) return type_easing_style is begin
		return type_easing_style'value (easing);
	end;

	function to_string (easing : in type_easing_style) return string is begin
		return to_lower (type_easing_style'image (easing));
	end;


-- 	procedure log_plated_millings (
-- 		millings 		: in type_plated_millings;
-- 		log_threshold	: in et_string_processing.type_log_level)
-- 		is
-- -- 		use pac_pcb_contour_lines;
-- -- 		use pac_pcb_contour_arcs;
-- -- 		use pac_pcb_contour_circles;
-- -- 		
-- -- 		procedure line (cursor : in pac_pcb_contour_lines.cursor) is begin
-- -- 			line_pcb_contour_properties (cursor, log_threshold);
-- -- 		end;
-- -- 
-- -- 		procedure arc (cursor : in pac_pcb_contour_arcs.cursor) is begin
-- -- 			arc_pcb_contour_properties (cursor, log_threshold);
-- -- 		end;
-- -- 
-- -- 		procedure circle (cursor : in pac_pcb_contour_circles.cursor) is begin
-- -- 			circle_pcb_contour_properties (cursor, log_threshold);
-- -- 		end;
-- 		
-- 	begin -- log_plated_millings
-- 		null;
-- -- CS
-- -- 		iterate (millings.lines, line'access);
-- -- 		iterate (millings.arcs, arc'access);
-- -- 		iterate (millings.circles, circle'access);
-- 	end log_plated_millings;

	function to_string (segment : in type_conductor_line_segment)
		return string
	is begin
		return ("line segment:" 
			& " edge left:" & to_string (segment.left_edge)
			& " cap end" & to_string (segment.cap_end)
			& " edge right:" & to_string (segment.right_edge)
			& " cap start:" & to_string (segment.cap_start));
	end to_string;

	
	function to_line_segment (line : in type_conductor_line)
		return type_conductor_line_segment
	is
		result : type_conductor_line_segment;
		direction : constant type_rotation := get_direction (line);
		distance : constant type_track_width := line.width * 0.5;		
	begin
		result.left_edge := type_line (line);
		move_by (result.left_edge, add (direction, +90.0), distance);

		result.right_edge := type_line (line);
		move_by (result.right_edge, add (direction, -90.0), distance);

		-- cap on the start of segment
		result.cap_start.center := line.start_point;
		result.cap_start.start_point := result.left_edge.start_point;
		result.cap_start.end_point := result.right_edge.start_point;
		result.cap_start.direction := CCW;

		-- cap on the end of the segment
		result.cap_end.center := line.end_point;
		result.cap_end.start_point := result.left_edge.end_point;
		result.cap_end.end_point := result.right_edge.end_point;
		result.cap_end.direction := CW;

		return result;
	end to_line_segment;


	function get_left_edge (segment : in type_conductor_line_segment)
		return type_line
	is begin
		return segment.left_edge;
	end get_left_edge;

	function get_right_edge (segment : in type_conductor_line_segment)
		return type_line
	is begin
		return segment.right_edge;
	end get_right_edge;
	
	function get_start_cap (segment : in type_conductor_line_segment)
		return type_arc
	is begin
		return segment.cap_start;
	end get_start_cap;

	function get_end_cap (segment : in type_conductor_line_segment)
		return type_arc
	is begin
		return segment.cap_end;
	end get_end_cap;


	function get_shortest_distance (
		point	: in type_point;
		segment	: in type_conductor_line_segment)
		return type_distance
	is 
		result : type_distance := zero;

		type type_segment_area is new type_polygon_base with null record;
		polygon : type_segment_area;

		length_left_edge  : constant type_distance_positive := get_length (segment.left_edge);
		length_right_edge : constant type_distance_positive := get_length (segment.right_edge);

		procedure build_polygon is begin
			if length_left_edge = zero and length_right_edge = zero then
				-- rare case: the segment has no straight section between the
				-- start and end cap. It is basically a circle.
				declare
					s : type_polygon_segments := (circular => true, others => <>);
				begin
					s.circle.center := segment.cap_start.center;
					s.circle.radius := get_radius_start (segment.cap_start);
					polygon.contours := s;
				end;
			else
				-- the most common case: the segment has a straight section between
				-- its start and end cap:
				declare
					use pac_polygon_segments;
					s : type_polygon_segments := (circular => false, others => <>);
				begin
					append (s.segments, (LINE, segment.left_edge));
					append (s.segments, (ARC, segment.cap_end));
					append (s.segments, (LINE, type_line (reverse_line (segment.right_edge))));
					append (s.segments, (ARC, type_arc (reverse_arc (segment.cap_start))));
					polygon.contours := s;
				end;
			end if;			
		end build_polygon;

		distance : type_distance_polar;
	begin
		-- build a polygon from the given segment:
		build_polygon;

		distance := get_shortest_distance (polygon, point);
		--log (text => to_string (polygon));
		--log (text => "p" & to_string (point));
		--log (text => "d" & to_string (get_absolute (distance)));

		case in_polygon_status (polygon, point).status is
			when INSIDE =>
				result := - get_absolute (distance);
				
			when OUTSIDE =>
				result := get_absolute (distance);
		end case;
		
		return result;
	end get_shortest_distance;



	function to_string (segment : in type_conductor_arc_segment)
		return string
	is begin
		return ("arc segment:" 
			& " outer edge:" & to_string (segment.outer_edge)
			& " cap end" & to_string (segment.cap_end)
			& " inner edge:" & to_string (segment.inner_edge)
			& " cap start:" & to_string (segment.cap_start));
	end to_string;

	
	function to_arc_segment (arc : in type_conductor_arc)
		return type_conductor_arc_segment
	is
		arc_n : type_conductor_arc := arc;
		arc_i, arc_o : type_arc_angles;
		
		center_radius : constant type_distance_positive := get_radius_start (arc_n);
		half_width : constant type_distance_positive := arc_n.width * 0.5;
		inner_radius, outer_radius : type_distance_positive;
		result : type_conductor_arc_segment;		
	begin
		-- normalize given arc so that it runs CW:
		if arc_n.direction = CCW then
			reverse_arc (arc_n);
		end if;

		-- set radii
		inner_radius := center_radius - half_width;
		outer_radius := center_radius + half_width;


		-- set outer edge:
		arc_o := to_arc_angles (arc_n);
		arc_o.radius := outer_radius;
		result.outer_edge := type_arc (to_arc (arc_o));

		-- set inner edge:
		arc_i := to_arc_angles (reverse_arc (arc_n));
		arc_i.radius := inner_radius;
		result.inner_edge := type_arc (to_arc (arc_i));

		-- set start and end points of caps:
		-- cap at start point:
		result.cap_start.start_point := result.inner_edge.end_point;
		result.cap_start.end_point   := result.outer_edge.start_point;
		result.cap_start.direction := CW;
		result.cap_start.center := arc_n.start_point;
		
		-- cap at end point:
		result.cap_end.start_point := result.outer_edge.end_point;
		result.cap_end.end_point   := result.inner_edge.start_point;
		result.cap_end.direction := CW;
		result.cap_end.center := arc_n.end_point;

		return result;
	end to_arc_segment;

	
	function get_inner_edge (segment : in type_conductor_arc_segment)
		return type_arc
	is begin
		return segment.inner_edge;
	end get_inner_edge;

	function get_outer_edge (segment : in type_conductor_arc_segment)
		return type_arc
	is begin
		return segment.outer_edge;
	end get_outer_edge;

	function get_start_cap (segment : in type_conductor_arc_segment)
		return type_arc
	is begin
		return segment.cap_start;
	end get_start_cap;

	function get_end_cap (segment : in type_conductor_arc_segment)
		return type_arc
	is begin
		return segment.cap_end;
	end get_end_cap;

	
	function get_shortest_distance (
		point	: in type_point;
		segment	: in type_conductor_arc_segment)
		return type_distance
	is 
		result : type_distance := zero;

		type type_segment_area is new type_polygon_base with null record;
		polygon : type_segment_area;

		procedure build_polygon is 
			use pac_polygon_segments;
			s : type_polygon_segments := (circular => false, others => <>);
		begin
			append (s.segments, (ARC, segment.outer_edge));
			append (s.segments, (ARC, segment.cap_end));
			append (s.segments, (ARC, segment.inner_edge));
			append (s.segments, (ARC, segment.cap_start));
			polygon.contours := s;
		end build_polygon;

		distance : type_distance_polar;
	begin
		-- build a polygon from the given segment:
		build_polygon;

		distance := get_shortest_distance (polygon, point);

		case in_polygon_status (polygon, point).status is
			when INSIDE =>
				result := - get_absolute (distance);
				
			when OUTSIDE =>
				result := get_absolute (distance);
		end case;
		
		return result;
	end get_shortest_distance;

	
	
	
	function to_string (appearance : in type_package_appearance) return string is begin
		return to_lower (type_package_appearance'image (appearance));
	end;

	function to_appearance (appearance : in string) return type_package_appearance is begin
		return type_package_appearance'value (appearance);
	end;
	
	
	function to_string (circle : in type_fillable_circle) return string is begin
		case circle.filled is
			when NO =>
				return
					pac_shapes.to_string (type_circle (circle)) &
					latin_1.space & et_text.keyword_line_width & to_string (circle.border_width);

			when YES =>
				case circle.fill_style is
					when SOLID =>
						return 
							pac_shapes.to_string (type_circle (circle)) &
							latin_1.space & keyword_fill_style & latin_1.space & to_string (circle.fill_style);

					when HATCHED =>
						return
							pac_shapes.to_string (type_circle (circle)) &
							latin_1.space & keyword_fill_style & latin_1.space & to_string (circle.fill_style) &
							latin_1.space & keyword_hatching_line_width & to_string (circle.hatching.line_width) &
							latin_1.space & keyword_hatching_line_spacing & to_string (circle.hatching.spacing);
				end case;
		end case;
	end;
	
	function to_string (
		description : in pac_package_description.bounded_string;
		verbose		: in boolean := false) return string is
	begin
		if verbose then
			return "description '" & pac_package_description.to_string (description) & "'";
		else
			return pac_package_description.to_string (description);
		end if;
	end to_string;

	function to_package_description (description : in string) 
		return pac_package_description.bounded_string 
	is begin
		return pac_package_description.to_bounded_string (description);
	end to_package_description;

	function locate_package_model (model_name : in pac_package_model_file_name.bounded_string) -- ../lbr/smd/SO15.pac
		return pac_packages_lib.cursor 
	is begin
		return pac_packages_lib.find (packages, model_name);
	end;
	
	function is_real (package_name : in pac_package_model_file_name.bounded_string) return boolean is
	-- Returns true if the given package is real (means it has a height).
		use pac_packages_lib;
		cursor : pac_packages_lib.cursor;
	begin
		cursor := find (packages, package_name);

		if element (cursor).appearance = REAL then
			return true;
		else
			return false;
		end if;
	end is_real;

	function terminal_properties (
		cursor		: in pac_packages_lib.cursor;
		terminal	: in pac_terminal_name.bounded_string) -- H4, 14
		return type_terminals.cursor is
	-- Returns a cursor to the requested terminal (with all its properties) within the given package model.
		terminal_cursor : type_terminals.cursor;

		procedure query_terminals (
			model_name	: in pac_package_model_file_name.bounded_string;
			model		: in type_package_lib) is
			use type_terminals;
		begin
			terminal_cursor := find (model.terminals, terminal);
		end;
		
	begin -- terminal_position
		pac_packages_lib.query_element (
			position	=> cursor,
			process		=> query_terminals'access);

		return terminal_cursor;
	end terminal_properties;
	
	procedure line_conductor_properties (
		face			: in type_face;
		cursor			: in pac_conductor_lines.cursor;
		log_threshold 	: in et_string_processing.type_log_level) 
	is
		line : type_conductor_line;
	begin
		line := element (cursor);
		log (text => "conductor line face" & to_string (face) & latin_1.space 
			 & to_string (type_line (line))
			 & " width" & to_string (line.width), level => log_threshold);
	end line_conductor_properties;

	procedure arc_conductor_properties (
		face			: in type_face;
		cursor			: in pac_conductor_arcs.cursor;
		log_threshold 	: in et_string_processing.type_log_level) 
	is
		arc : type_conductor_arc;
	begin
		arc := element (cursor);
		log (text => "conductor arc face" & to_string (face) & latin_1.space 
			 & to_string (type_arc (arc))
			 & " width" & to_string (arc.width), level => log_threshold);
	end arc_conductor_properties;
	
	procedure circle_conductor_properties (
		face			: in type_face;
		cursor			: in pac_conductor_circles.cursor;
		log_threshold 	: in et_string_processing.type_log_level) 
	is begin
		case element (cursor).filled is
			when NO =>
				log (text => "conductor circle face" & to_string (face) & latin_1.space 
					& to_string (type_circle (element (cursor)))
					& " filled" & to_string (element (cursor).filled)
					& " border width" & to_string (element (cursor).border_width),
					level => log_threshold);

			when YES =>
				case element (cursor).fill_style is
					when SOLID =>
						log (text => "conductor circle face" & to_string (face) & latin_1.space 
							& to_string (type_circle (element (cursor)))
							& " fill style" & to_string (element (cursor).fill_style),
							level => log_threshold);

					when HATCHED =>
						log (text => "conductor circle face" & to_string (face) & latin_1.space 
							& to_string (type_circle (element (cursor)))
							& " fill style" & to_string (element (cursor).fill_style),
							-- CS show hatching details
							level => log_threshold);
						
				end case;
		end case;		
	end circle_conductor_properties;

	
-- PROPERTIES OF OBJECTS IN SILK SCREEN
	procedure line_silk_screen_properties (
		face			: in type_face;
		cursor			: in pac_silk_lines.cursor;
		log_threshold 	: in et_string_processing.type_log_level)
	is
		use pac_silk_lines;
		line : type_silk_line;
	begin
		line := element (cursor);
		log (text => "silk screen line face" & to_string (face) & latin_1.space 
			 & to_string (type_line (line))
			 & " width" & to_string (line.width), level => log_threshold);
	end line_silk_screen_properties;

	procedure arc_silk_screen_properties (
		face			: in type_face;
		cursor			: in pac_silk_arcs.cursor;
		log_threshold 	: in et_string_processing.type_log_level)
	is
		use pac_silk_arcs;
		arc : type_silk_arc;
	begin
		arc := element (cursor);
		log (text => "silk screen arc face" & to_string (face) & latin_1.space 
			 & to_string (type_arc (arc))
			 & " width" & to_string (arc.width), level => log_threshold);
	end arc_silk_screen_properties;
	
	procedure circle_silk_screen_properties (
		face			: in type_face;
		cursor			: in pac_silk_circles.cursor;
		log_threshold 	: in et_string_processing.type_log_level)
	is
		use pac_silk_circles;
	begin
		log (text => "silk screen circle face" & to_string (face)
			 & to_string (element (cursor)),
			level => log_threshold);
	end;

	procedure placeholder_silk_screen_properties (
	-- Logs the properties of the given silk screen placeholder
		face			: in type_face;
		cursor			: in pac_text_placeholders.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is
		use pac_text_placeholders;
		placeholder : type_text_placeholder;
	begin
		placeholder := element (cursor);
		log (text => "silk screen placeholder face" & to_string (face)
			 & " for " & to_string (placeholder.meaning), level => log_threshold);
		
		log_indentation_up;
		log (text => text_properties (type_text (placeholder)), level => log_threshold + 1);
		log_indentation_down;
	end placeholder_silk_screen_properties;
	
	procedure text_silk_screen_properties (
	-- Logs the properties of the given silk screen text
		face			: in type_face;
		cursor			: in pac_texts_with_content.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is
		use et_text.pac_text_content;
		use pac_texts_with_content;
		text : type_text_with_content;
	begin
		text := element (cursor);
		log (text => "silk screen text face" & to_string (face) & latin_1.space
			 & "content '" & to_string (text.content) & "'", level => log_threshold);

		log_indentation_up;
		log (text => text_properties (type_text (text)), level => log_threshold + 1);
		log_indentation_down;
	end text_silk_screen_properties;


	
-- PROPERTIES OF OBJECTS IN ASSEMBLY DOCUMENTATION
	procedure line_assy_doc_properties (
		face			: in type_face;
		cursor			: in pac_doc_lines.cursor;
		log_threshold 	: in et_string_processing.type_log_level)
	is
		use pac_doc_lines;
		line : type_doc_line;
	begin
		line := element (cursor);
		log (text => "assembly doc line face" & to_string (face) & latin_1.space
			 & to_string (type_line (line))
			 & " width" & to_string (line.width), level => log_threshold);
	end line_assy_doc_properties;

	procedure arc_assy_doc_properties (
		face			: in type_face;
		cursor			: in pac_doc_arcs.cursor;
		log_threshold 	: in et_string_processing.type_log_level)
	is
		use pac_doc_arcs;
		arc : type_doc_arc;
	begin
		arc := element (cursor);
		log (text => "assembly doc arc face" & to_string (face) & latin_1.space 
			 & to_string (type_arc (arc))
			 & " width" & to_string (arc.width), level => log_threshold);
	end arc_assy_doc_properties;

	procedure circle_assy_doc_properties (
		face			: in type_face;
		cursor			: in pac_doc_circles.cursor;
		log_threshold 	: in et_string_processing.type_log_level)
	is
		use pac_doc_circles;
	begin
		log (text => "assembly doc circle face" & to_string (face) & latin_1.space 
			 & to_string (element (cursor)),
			level => log_threshold);
	end;

	procedure placeholder_assy_doc_properties (
	-- Logs the properties of the given assembly documentation placeholder
		face			: in type_face;
		cursor			: in pac_text_placeholders.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is
		use pac_text_placeholders;
		placeholder : type_text_placeholder;
	begin
		placeholder := element (cursor);
		log (text => "assembly doc placeholder face" & to_string (face)
			 & " for " & to_string (placeholder.meaning), level => log_threshold);

		log_indentation_up;
		log (text => text_properties (type_text (placeholder)), level => log_threshold + 1);
		log_indentation_down;
	end placeholder_assy_doc_properties;

	procedure text_assy_doc_properties (
	-- Logs the properties of the given assembly documentation text
		face			: in type_face;
		cursor			: in pac_texts_with_content.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is
		use et_text.pac_text_content;
		use pac_texts_with_content;
		text : type_text_with_content;
	begin
		text := element (cursor);
		log (text => "assembly doc text face" & to_string (face) & latin_1.space
			 & "content '" & to_string (text.content) & "'", level => log_threshold);

		log_indentation_up;
		log (text => text_properties (type_text (text)), level => log_threshold + 1);
		log_indentation_down;
	end text_assy_doc_properties;


	
	
-- PROPERTIES OF OBJECTS IN KEEPOUT
	procedure line_keepout_properties (
		face			: in type_face;
		cursor			: in pac_keepout_lines.cursor;
		log_threshold 	: in et_string_processing.type_log_level) 
	is
		use pac_keepout_lines;
		line : type_keepout_line;
	begin
		line := element (cursor);
		log (text => "keepout (courtyard) line face" & to_string (face) & latin_1.space
			 & to_string (type_line (line)), level => log_threshold);
	end line_keepout_properties;

	procedure arc_keepout_properties (
		face			: in type_face;
		cursor			: in pac_keepout_arcs.cursor;
		log_threshold 	: in et_string_processing.type_log_level)
	is
		use pac_keepout_arcs;
		arc : type_keepout_arc;
	begin
		arc := element (cursor);
		log (text => "keepout (courtyard) arc face" & to_string (face) & latin_1.space 
			 & to_string (type_arc (arc)), level => log_threshold);
	end arc_keepout_properties;

	procedure circle_keepout_properties (
		face			: in type_face;
		cursor			: in pac_keepout_circles.cursor;
		log_threshold 	: in et_string_processing.type_log_level)
	is
		use pac_keepout_circles;
	begin
		log (text => "keepout circle face" & to_string (face) & latin_1.space 
			 & to_string (element (cursor)),
			level => log_threshold);
	end;


-- PROPERTIES OF OBJECTS IN STOP MASK
	procedure arc_stop_mask_properties (
		face			: in type_face;
		cursor			: in pac_stop_arcs.cursor;
		log_threshold 	: in et_string_processing.type_log_level) 
	is
		use pac_stop_arcs;
		arc : type_stop_arc;
	begin
		arc := element (cursor);
		log (text => "stop mask arc face" & to_string (face) & latin_1.space 
			 & to_string (type_arc (arc))
			 & " width" & to_string (arc.width),
			 level => log_threshold);
	end arc_stop_mask_properties;

	procedure circle_stop_mask_properties (
		face			: in type_face;
		cursor			: in pac_stop_circles.cursor;
		log_threshold 	: in et_string_processing.type_log_level) 
	is
		use pac_stop_circles;
	begin
		log (text => "stop mask circle face" & to_string (face) & latin_1.space 
			& to_string (element (cursor)),
			level => log_threshold);
	end;

	procedure line_stop_mask_properties (
		face			: in type_face;
		cursor			: in pac_stop_lines.cursor;
		log_threshold 	: in et_string_processing.type_log_level) 
	is
		use pac_stop_lines;
		line : type_stop_line;
	begin
		line := element (cursor);
		log (text => "stop mask line face" & to_string (face) & latin_1.space
			 & to_string (type_line (line))
			 & " width" & to_string (line.width),
			 level => log_threshold);
	end line_stop_mask_properties;

	procedure text_stop_mask_properties (
	-- Logs the properties of the given stop mask text
		face			: in type_face;
		cursor			: in pac_texts_with_content.cursor;
		log_threshold 	: in et_string_processing.type_log_level) 
	is
		use et_text.pac_text_content;
		use pac_texts_with_content;
		text : type_text_with_content;
	begin
		text := element (cursor);
		log (text => "stop mask text face" & to_string (face) & latin_1.space
			 & "content '" & to_string (text.content) & "'", level => log_threshold);

		log_indentation_up;
		log (text => text_properties (type_text (text)), level => log_threshold + 1);
		log_indentation_down;
	end text_stop_mask_properties;


-- PROPERTIES OF OBJECTS IN SOLDER PASTE / STENCIL
	procedure arc_stencil_properties (
		face			: in type_face;
		cursor			: in pac_stencil_arcs.cursor;
		log_threshold 	: in et_string_processing.type_log_level) 
	is
		use pac_stencil_arcs;
		arc : type_stencil_arc;
	begin
		arc := element (cursor);
		log (text => "solder paste (stencil) arc face" & to_string (face) & latin_1.space 
			 & to_string (type_arc (arc))
			 & " width" & to_string (arc.width),
			 level => log_threshold);
	end arc_stencil_properties;

	procedure circle_stencil_properties (
		face			: in type_face;
		cursor			: in pac_stencil_circles.cursor;
		log_threshold 	: in et_string_processing.type_log_level) 
	is
		use pac_stencil_circles;
	begin
		log (text => "solder paste (stencil) circle face" & to_string (face) & latin_1.space 
			& to_string (element (cursor)),
			level => log_threshold);
	end;

	procedure line_stencil_properties (
		face			: in type_face;
		cursor			: in pac_stencil_lines.cursor;
		log_threshold 	: in et_string_processing.type_log_level) 
	is
		use pac_stencil_lines;
		line : type_stencil_line;
	begin
		line := element (cursor);
		log (text => "solder paste (stencil) line face" & to_string (face) & latin_1.space
			 & to_string (type_line (line))
			 & " width" & to_string (line.width),
			 level => log_threshold);
	end line_stencil_properties;
	
	
	
-- PROPERTIES OF OBJECTS IN ROUTE RESTRICT
	procedure line_route_restrict_properties (
	-- Logs the properties of the given line of route restrict
		face			: in type_face;
		cursor			: in pac_route_restrict_lines.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is
		use pac_route_restrict_lines;
		line : type_route_restrict_line;
	begin
		line := element (cursor);
		log (text => "route restrict line face" & to_string (face) & latin_1.space
			 & to_string (type_line (line)), level => log_threshold);
	end line_route_restrict_properties;

	procedure arc_route_restrict_properties (
		face			: in type_face;
		cursor			: in pac_route_restrict_arcs.cursor;
		log_threshold 	: in et_string_processing.type_log_level)
	is
		use pac_route_restrict_arcs;
		arc : type_route_restrict_arc;
	begin
		arc := element (cursor);
		log (text => "route restrict arc face" & to_string (face) & latin_1.space 
			 & to_string (type_arc (arc)), level => log_threshold);
	end arc_route_restrict_properties;


	-- CS procedure circle_route_restrict_properties
	

-- PROPERTIES OF OBJECTS IN VIA RESTRICT	
	procedure line_via_restrict_properties (
		face			: in type_face;
		cursor			: in pac_via_restrict_lines.cursor;
		log_threshold 	: in et_string_processing.type_log_level) 
	is
		use pac_via_restrict_lines;
		line : type_via_restrict_line;
	begin
		line := element (cursor);
		log (text => "via restrict line face" & to_string (face) & latin_1.space
			 & to_string (type_line (line)), level => log_threshold);
	end line_via_restrict_properties;

	procedure arc_via_restrict_properties (
		face			: in type_face;
		cursor			: in pac_via_restrict_arcs.cursor;
		log_threshold 	: in et_string_processing.type_log_level)
	is
		use pac_via_restrict_arcs;
		arc : type_via_restrict_arc;
	begin
		arc := element (cursor);
		log (text => "via restrict arc face" & to_string (face) & latin_1.space 
			 & to_string (type_arc (arc)), level => log_threshold);
	end arc_via_restrict_properties;

	-- CS procedure circle_via_restrict_properties

	
-- PROPERTIES OF OBJECTS IN BOARD CONTOUR / OUTLINE / EDGE CUTS
	--procedure line_pcb_contour_properties (
		--cursor			: in pac_pcb_contour_lines.cursor;
		--log_threshold 	: in et_string_processing.type_log_level)
	--is
		--use pac_pcb_contour_lines;
		--line : type_pcb_contour_line;
	--begin
		--line := element (cursor);
		--log (text => "PCB contour (edge cuts / outline) line" & latin_1.space
			 --& to_string (type_line (line)), level => log_threshold);
	--end line_pcb_contour_properties;

	--procedure arc_pcb_contour_properties (
		--cursor			: in pac_pcb_contour_arcs.cursor;
		--log_threshold 	: in et_string_processing.type_log_level)
	--is
		--use pac_pcb_contour_arcs;
		--arc : type_pcb_contour_arc;
	--begin
		--arc := element (cursor);
		--log (text => "PCB contour (edge cuts / outline) arc" & latin_1.space 
			 --& to_string (type_arc (arc)), level => log_threshold);
	--end arc_pcb_contour_properties;

	--procedure circle_pcb_contour_properties (
		--cursor			: in pac_pcb_contour_circles.cursor;
		--log_threshold 	: in et_string_processing.type_log_level)
	--is
		--use pac_pcb_contour_circles;
		--circle : type_pcb_contour_circle;
	--begin
		--circle := element (cursor);
		--log (text => "PCB contour (edge cuts / outline) circle" & latin_1.space 
			--& to_string (type_circle (circle)), level => log_threshold);
	--end circle_pcb_contour_properties;


	
	
	
	

-- 	procedure terminal_properties (
-- 	-- Logs the properties of the given terminal.
-- 		terminal		: in type_terminal;
-- 		name			: in pac_terminal_name.bounded_string;
-- 		log_threshold 	: in et_string_processing.type_log_level) is
-- 		use et_pcb_coordinates;
-- 		log_threshold_1 : type_log_level := log_threshold + 1;
-- 
-- -- 		use type_pad_lines;
-- -- 		use type_pad_arcs;
-- -- 		use type_pad_circles;
-- -- 		use type_pad_polygons;		
-- 		
-- -- 		procedure line (cursor : in type_pad_lines.cursor) is begin
-- -- 			log (text => to_string (shapes.type_line (element (cursor))), level => log_threshold + 1);
-- -- 		end line;
-- -- 
-- -- 		procedure arc (cursor : in type_pad_arcs.cursor) is begin
-- -- 			log (text => to_string (shapes.type_arc (element (cursor))), level => log_threshold + 1);
-- -- 		end arc;
-- -- 		
-- -- 		procedure circle (cursor : in type_pad_circles.cursor) is begin
-- -- 			log (text => to_string (shapes.type_circle (element (cursor))), level => log_threshold + 1);
-- -- 		end circle;
-- -- 
-- -- 		procedure polygon (cursor : in type_pad_polygons.cursor) is 
-- -- 			use type_polygon_points;
-- -- 			points : type_polygon_points.set := element (cursor).corners;
-- -- 
-- -- 			procedure point (cursor : in type_polygon_points.cursor) is begin
-- -- 				log (text => to_string (element (cursor)), level => log_threshold + 1);	
-- -- 			end point;
-- -- 	
-- -- 		begin -- polygon
-- -- 			log (text => "polygon with corners", level => log_threshold + 1);
-- -- 			log_indentation_up;
-- -- 			iterate (points, point'access);
-- -- 			log_indentation_down;
-- -- 		end polygon;
-- 			
-- 	begin -- terminal_properties
-- 		log (text => "terminal name " & to_string (name)
-- 			& " technology" & to_string (terminal.technology)
-- 			& to_string (type_point (terminal.position))
-- 			& " rotation" & to_string (rot (terminal.position)),
-- 			level => log_threshold);
-- 
-- 		log_indentation_up;
-- 
-- 		case terminal.technology is
-- 			when THT => 
-- 				
-- 				-- log pad_shape_top/bottom
-- 				log (text => "pad contour top", level => log_threshold + 1);
-- -- 				iterate (terminal.pad_shape_tht.top.lines, line'access);
-- -- 				iterate (terminal.pad_shape_tht.top.arcs, arc'access);
-- -- 				iterate (terminal.pad_shape_tht.top.circles, circle'access);
-- -- 				iterate (terminal.pad_shape_tht.top.polygons, polygon'access);
-- 
-- 				log (text => "pad contour bottom", level => log_threshold + 1);
-- -- 				iterate (terminal.pad_shape_tht.bottom.lines, line'access);
-- -- 				iterate (terminal.pad_shape_tht.bottom.arcs, arc'access);
-- -- 				iterate (terminal.pad_shape_tht.bottom.circles, circle'access);
-- -- 				iterate (terminal.pad_shape_tht.bottom.polygons, polygon'access);
-- 				
-- 				log (text => "copper width of inner layers" & to_string (terminal.width_inner_layers), level => log_threshold_1);
-- 
-- 				case terminal.tht_hole is
-- 					when DRILLED =>
-- 						log (text => "drill" & to_string (terminal.drill_size), level => log_threshold_1); 
-- 					when MILLED =>
-- 						if log_level >= log_threshold_1 then
-- 							log (text => "plated milling contour ");
-- 							log_indentation_up;
-- 								log_plated_millings (terminal.millings, log_threshold_1);
-- 							log_indentation_down;
-- 						end if;
-- 				end case;
-- 				
-- 			when SMT => 
-- 				
-- 				-- log pad_shape
-- 				log (text => "pad contour", level => log_threshold + 1);
-- -- 				iterate (terminal.pad_shape.lines, line'access);
-- -- 				iterate (terminal.pad_shape.arcs, arc'access);
-- -- 				iterate (terminal.pad_shape.circles, circle'access);
-- -- 				iterate (terminal.pad_shape.polygons, polygon'access);
-- 				
-- 				log (text => "face" & to_string (terminal.face), level => log_threshold_1);
-- 				log (text => "stop mask" & to_string (terminal.stop_mask), level => log_threshold_1);
-- 				log (text => "solder paste" & to_string (terminal.solder_paste), level => log_threshold_1);
-- 		end case;
-- 
-- 		log_indentation_down;
-- 	end terminal_properties;


	



	
end et_packages;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
