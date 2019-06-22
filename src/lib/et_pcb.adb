------------------------------------------------------------------------------
--                                                                          --
--                           SYSTEM ET PCB                                  --
--                                                                          --
--                                 ET                                       --
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

--   For correct displaying set tab width in your edtior to 4.

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

	function to_string (signal_layer : in type_signal_layer) return string is begin
		--return trim (type_signal_layer'image (signal_layer), left);
		return type_signal_layer'image (signal_layer);
	end to_string;

	function to_signal_layer (layer : in string) return type_signal_layer is begin
		return type_signal_layer'value (layer);
	end to_signal_layer;
	
	procedure validate_text_size (size : in type_distance) is
	-- Checks whether given text size is in range of type_text_size.
	begin
		if size not in type_text_size then
			log (ERROR, "text size invalid ! Allowed range is" 
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
			log (ERROR, "line width invalid ! Allowed range is" 
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
			log (ERROR, "line width invalid ! Allowed range is" 
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
			log (ERROR, "drill size invalid ! Allowed range is" 
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
			log (ERROR, "pad size invalid ! Allowed range is" 
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
	
	procedure validate_track_clearance (clearance : in type_distance) is
	-- Checks whether the given track clearance is in range of type_track_clearance.
	begin
		if clearance not in type_track_clearance then
			log (ERROR, "track clearance invalid ! Allowed range is" 
				 & to_string (type_track_clearance'first) & " .."
				 & to_string (type_track_clearance'last),
				 console => true);
			raise constraint_error;
		end if;
	end validate_track_clearance;

	procedure validate_track_width (track_width : in type_distance) is
	-- Checks whether the given width is in range of type_track_width.
	begin
		if track_width not in type_track_width then
			log (ERROR, "track width invalid ! Allowed range is" 
				 & to_string (type_track_width'first) & " .."
				 & to_string (type_track_width'last),
				 console => true);
			raise constraint_error;
		end if;
	end validate_track_width;

	procedure validate_restring_width (restring_width : in type_distance) is
	-- Checks whether the given restring width is in range of type_restring_width.	
	begin
		if restring_width not in type_restring_width then
			log (ERROR, "restring width invalid ! Allowed range is" 
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

	function to_net_class_name (net_class_name : in string) return type_net_class_name.bounded_string is
	begin
		return type_net_class_name.to_bounded_string (net_class_name);
	end to_net_class_name;
	
	function to_string (class_description : in type_net_class_description.bounded_string) return string is
	begin
		return type_net_class_description.to_string (class_description);
	end to_string;

	function to_net_class_description (class_description : in string) return type_net_class_description.bounded_string is
	begin
		return type_net_class_description.to_bounded_string (class_description);
	end to_net_class_description;
	
	function text_properties (text : in type_text) return string is
	-- Returns the properties of the given text in a long single string.
		use et_coordinates;
	begin
		return to_string (text.position) & latin_1.space
			& "size (width" & axis_separator & "height)" 
			& to_string (text.dimensions.width) & latin_1.space & axis_separator & to_string (text.dimensions.height)
			& " line width" & to_string (text.line_width)
			& to_string (get_angle (text.position), preamble => true)
			& et_libraries.to_string (text.alignment)
			-- CS & " hidden " & boolean'image (text.hidden)
			;
	end text_properties;
	
	function to_string (text_meaning : in type_text_meaning_package) return string is begin
		return latin_1.space & to_lower (type_text_meaning_package'image (text_meaning));
	end to_string;

	function to_text_meaning (text_meaning : in string) return type_text_meaning_package is begin
		return type_text_meaning_package'value (text_meaning);
	end to_text_meaning;

	function to_string (layer : in type_placeholder_package_layer) return string is begin
		return latin_1.space & to_lower (type_placeholder_package_layer'image (layer));
	end to_string;

	function to_layer (layer : in string) return type_placeholder_package_layer is begin
		return type_placeholder_package_layer'value (layer);
	end to_layer;
	
	function to_string (meaning : in type_text_meaning_copper) return string is begin
		return latin_1.space & to_lower (type_text_meaning_copper'image (meaning));
	end to_string;

	function to_meaning (meaning : in string) return type_text_meaning_copper is begin
		return type_text_meaning_copper'value (meaning);
	end to_meaning;
	
	function to_string (meaning : in type_text_meaning_pcb) return string is begin
		return latin_1.space & to_lower (type_text_meaning_pcb'image (meaning));
	end to_string;

	function to_meaning (meaning : in string) return type_text_meaning_pcb is begin
		return type_text_meaning_pcb'value (meaning);
	end to_meaning;
	
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
		
		procedure line (cursor : in type_pcb_contour_lines.cursor) is begin
			line_pcb_contour_properties (cursor, log_threshold);
		end;

		procedure arc (cursor : in type_pcb_contour_arcs.cursor) is begin
			arc_pcb_contour_properties (cursor, log_threshold);
		end;

		procedure circle (cursor : in type_pcb_contour_circles.cursor) is begin
			circle_pcb_contour_properties (cursor, log_threshold);
		end;
		
	begin -- log_plated_millings
		iterate (millings.lines, line'access);
		iterate (millings.arcs, arc'access);
		iterate (millings.circles, circle'access);
	end log_plated_millings;
	
		
	
	function to_string (appearance : in type_package_appearance) return string is begin
		return latin_1.space & to_lower (type_package_appearance'image (appearance));
	end to_string;

	function to_appearance (appearance : in string) return type_package_appearance is begin
		return type_package_appearance'value (appearance);
	end;
	
	function to_string (technology : in type_assembly_technology) return string is begin
		return latin_1.space & to_lower (type_assembly_technology'image (technology));
	end to_string;

	function to_assembly_technology (technology : in string) return type_assembly_technology is begin
		return type_assembly_technology'value (technology);
	end;
	
	function to_string (solder_paste : in type_solder_paste_status) return string is begin
		return latin_1.space & to_lower (type_solder_paste_status'image (solder_paste));
	end to_string;

	function to_solder_paste_status (solder_paste : in string) return type_solder_paste_status is begin
		return type_solder_paste_status'value (solder_paste);
	end;
	
	function to_string (stop_mask : in type_stop_mask_status) return string is begin
		return latin_1.space & to_lower (type_stop_mask_status'image (stop_mask));
	end to_string;

	function to_stop_mask_status (stop_mask : in string) return type_stop_mask_status is begin
		return type_stop_mask_status'value (stop_mask);
	end to_stop_mask_status;
	
	function to_string (tht_hole : in type_terminal_tht_hole) return string is begin
		return latin_1.space & to_lower (type_terminal_tht_hole'image (tht_hole));
	end to_string;

	function to_tht_hole (tht_hole : in string) return type_terminal_tht_hole is begin
		return type_terminal_tht_hole'value (tht_hole);
	end to_tht_hole;
	
	function to_string (filled : in type_filled) return string is begin
		return latin_1.space & to_lower (type_filled'image (filled));
	end to_string;

	function to_filled (filled : in string) return type_filled is begin
		return type_filled'value (filled);
	end to_filled;
	
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
			log (ERROR, "type of easing '" & easing & "' invalid !", console => true);
			raise constraint_error;
		end if;

		return easing_out;
	end to_corner_easing;

	function to_string (easing : in type_corner_easing) return string is
	begin
		return latin_1.space & to_lower (type_corner_easing'image (easing));
	end to_string;

	function to_string (polygon_pad_connection : in type_polygon_pad_connection) return string is begin
		return latin_1.space & to_lower (type_polygon_pad_connection'image (polygon_pad_connection));
	end to_string;

	function to_pad_connection (connection : in string) return type_polygon_pad_connection is begin
		return type_polygon_pad_connection'value (connection);
	end to_pad_connection;
	
	function to_string (polygon_pad_technology : in type_polygon_pad_technology) return string is begin
		return latin_1.space & to_lower (type_polygon_pad_technology'image (polygon_pad_technology));
	end to_string;

	function to_pad_technology (technology : in string) return type_polygon_pad_technology is begin
		return type_polygon_pad_technology'value (technology);
	end to_pad_technology;
	
	function to_string (fill_style : in type_fill_style) return string is
	begin
		return " " & to_lower (type_fill_style'image (fill_style));
	end to_string;

	function to_fill_style (fill_style : in string) return type_fill_style is
	begin
		return type_fill_style'value (fill_style);
	end to_fill_style;

	function to_string (locked : in type_locked) return string is begin
		return latin_1.space & to_lower (type_locked'image (locked));
	end to_string;

	function to_lock_status (locked : in string) return type_locked is begin
		return type_locked'value (locked);
	end to_lock_status;
	
	function to_string (line : in type_line_2d) return string is
	-- Returns the start and end point of the given line as string.
	begin
		return latin_1.space 
			& "start" & to_string (line.start_point) 
			& " end" & to_string (line.end_point);
	end to_string;

	function to_string (arc : in type_arc_2d) return string is
	-- Returns the start, end point and angle of the given arc as string.
	begin
		return latin_1.space 
			& "center" & to_string (arc.center) 
			& " start" & to_string (arc.start_point) 
			& " end" & to_string (arc.end_point);
	end to_string;

	function to_string (circle : in type_circle_2d) return string is
	-- Returns the center and radius of the given circle as string.
	begin
		return latin_1.space
			& "center" & to_string (circle.center) 
			& " radius" & to_string (circle.radius);
	end to_string;

	
	function to_string (
		description : in type_package_description.bounded_string;
		verbose		: in boolean := false) return string is
	begin
		if verbose then
			return "description '" & type_package_description.to_string (description) & "'";
		else
			return type_package_description.to_string (description);
		end if;
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
		return (" position" & to_string (type_point_2d (position))
			& " angle" & to_string (get_angle (position))
			& " face" & to_string (get_face (position)));
	end package_position;

	
	
-- PROPERTIES OF OBJECTS IN COPPER (NON ELECTRIC !!)
	procedure line_copper_properties (
	-- Logs the properties of the given line of copper
		face			: in type_face;
		cursor			: in type_copper_lines.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is
		use type_copper_lines;
		line : type_copper_line;
	begin
		line := element (cursor);
		log (text => "copper line face" & to_string (face) & latin_1.space 
			 & to_string (type_line_2d (line))
			 & " width" & to_string (line.width), level => log_threshold);
	end line_copper_properties;

	procedure arc_copper_properties (
	-- Logs the properties of the given arc of copper
		face			: in type_face;
		cursor			: in type_copper_arcs.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is
		use type_copper_arcs;
		arc : type_copper_arc;
	begin
		arc := element (cursor);
		log (text => "copper arc face" & to_string (face) & latin_1.space 
			 & to_string (type_arc_2d (arc))
			 & " width" & to_string (arc.width), level => log_threshold);
	end arc_copper_properties;
	
	procedure circle_copper_properties (
	-- Logs the properties of the given circle of copper
		face			: in type_face;
		cursor			: in type_copper_circles.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is
		use type_copper_circles;
		circle : type_copper_circle;
	begin
		circle := element (cursor);
		log (text => "copper circle face" & to_string (face) & latin_1.space 
			 & to_string (type_circle_2d (circle))
			 & " width" & to_string (circle.width)
			 & " filled" & to_string (circle.filled), level => log_threshold);
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
		log (text => "copper text signal layer" & to_string (text.layer) & latin_1.space
			& "content '" & to_string (text.content) & "'", level => log_threshold
			);

		log_indentation_up;
		log (text => text_properties (type_text (text)), level => log_threshold + 1);
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
		log (text => "segment " & et_pcb.to_string (type_line_2d (line)) &
			 " width" & to_string (line.width) &
			 " layer" & to_string (line.layer)
			 -- CS locked
			 , level => log_threshold);
	end route_line_properties;
	
	procedure route_via_properties (
	-- Logs the properties of the given via of a route
		cursor			: in type_vias.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is
		use type_vias;
		via : type_via;
	begin
		via := element (cursor);
		log (text => "via" & et_pcb.to_string (type_drill (via)) &
			 " restring_outer" & to_string (via.restring_outer) & -- outer layers
			 " restring_inner" & to_string (via.restring_inner) & -- inner layers
			 " layer_start" & to_string (via.layer_start) &
			 " layer_end" & to_string (via.layer_end)
			 -- CS locked
			 , level => log_threshold);
	end route_via_properties;

	procedure route_polygon_properties (
	-- Logs the properties of the given polygon of a route
		cursor			: in type_copper_polygons_signal.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is
		use type_copper_polygons_signal;
		use type_polygon_points;
		points : type_polygon_points.set;
		point_cursor : type_polygon_points.cursor;
	begin
		-- general stuff
		log (text => "polygon" & 
			 " " & text_polygon_signal_layer & to_string (element (cursor).layer) &
			 " " & text_polygon_width_min & to_string (element (cursor).width_min) &
			 " " & text_polygon_pad_connection & to_string (element (cursor).pad_connection) &
			 " " & text_polygon_priority_level & to_string (element (cursor).priority_level) &
			 " " & text_polygon_isolation_gap & to_string (element (cursor).isolation_gap) &
			 " " & text_polygon_corner_easing & to_string (element (cursor).corner_easing) &
			 " " & text_polygon_easing_radius & to_string (element (cursor).easing_radius),
			 level => log_threshold);

		log_indentation_up;
		
		-- type depended stuff
		case element (cursor).pad_connection is
			when THERMAL =>
				log (text => text_polygon_pad_technology & to_string (element (cursor).thermal_technology) &
					" " & text_polygon_thermal_width & to_string (element (cursor).thermal_width) &
					" " & text_polygon_thermal_gap & to_string (element (cursor).thermal_gap),
					level => log_threshold);

			when SOLID =>
				log (text => text_polygon_pad_technology & to_string (element (cursor).solid_technology),
					level => log_threshold);
				
			when NONE =>
				null;
		end case;

		-- corner points
		log (text => text_polygon_corner_points, level => log_threshold);
		points := element (cursor).corners;
		point_cursor := points.first;
		while point_cursor /= type_polygon_points.no_element loop
			log (text => to_string (element (point_cursor)), level => log_threshold);
			next (point_cursor);
		end loop;
		
		log_indentation_down;
	end route_polygon_properties;

	procedure floating_copper_polygon_properties (
	-- Logs the properties of the given floating copper polygon.
		cursor			: in type_copper_polygons_floating.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is
		use type_copper_polygons_floating;
		use type_polygon_points;
		points : type_polygon_points.set;
		point_cursor : type_polygon_points.cursor;
	begin
		-- general stuff
		log (text => "polygon" & 
			 " " & text_polygon_signal_layer & to_string (element (cursor).layer) &
			 " " & text_polygon_width_min & to_string (element (cursor).width_min) &
			 " " & text_polygon_corner_easing & to_string (element (cursor).corner_easing) &
			 " " & text_polygon_easing_radius & to_string (element (cursor).easing_radius),
			 level => log_threshold);

		log_indentation_up;
		
		-- corner points
		log (text => text_polygon_corner_points, level => log_threshold);
		points := element (cursor).corners;
		point_cursor := points.first;
		while point_cursor /= type_polygon_points.no_element loop
			log (text => to_string (element (point_cursor)), level => log_threshold);
			next (point_cursor);
		end loop;
		
		log_indentation_down;
	end floating_copper_polygon_properties;

	
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
		log (text => "silk screen line face" & to_string (face) & latin_1.space 
			 & to_string (type_line_2d (line))
			 & " width" & to_string (line.width), level => log_threshold);
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
		log (text => "silk screen arc face" & to_string (face) & latin_1.space 
			 & to_string (type_arc_2d (arc))
			 & " width" & to_string (arc.width), level => log_threshold);
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
		log (text => "silk screen circle face" & to_string (face) & latin_1.space 
			 & to_string (type_circle_2d (circle))
			 & " width" & to_string (circle.width)
			 & " filled" & to_string (circle.filled)
			 & " " & text_fill_style & to_string (circle.fill_style), level => log_threshold);

		-- if filled with a hatched pattern, output hatch line width and spacing
		if circle.fill_style = HATCHED then
			log (text => text_hatching_line_width & to_string (circle.hatching_line_width) & " "
				& text_hatching_spacing & to_string (circle.hatching_spacing), level => log_threshold);
		end if;
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
		log (text => "silk screen placeholder face" & to_string (face)
			 & " for " & to_string (placeholder.meaning), level => log_threshold);
		
		log_indentation_up;
		log (text => text_properties (type_text (placeholder)), level => log_threshold + 1);
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
		log (text => "silk screen text face" & to_string (face) & latin_1.space
			 & "content '" & to_string (text.content) & "'", level => log_threshold);

		log_indentation_up;
		log (text => text_properties (type_text (text)), level => log_threshold + 1);
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
		log (text => "assembly doc line face" & to_string (face) & latin_1.space
			 & to_string (type_line_2d (line))
			 & " width" & to_string (line.width), level => log_threshold);
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
		log (text => "assembly doc arc face" & to_string (face) & latin_1.space 
			 & to_string (type_arc_2d (arc))
			 & " width" & to_string (arc.width), level => log_threshold);
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
		log (text => "assembly doc circle face" & to_string (face) & latin_1.space 
			 & to_string (type_circle_2d (circle))
			 & " width" & to_string (circle.width)
			 & " filled" & to_string (circle.filled)
			 & " " & text_fill_style & to_string (circle.fill_style), level => log_threshold);

		-- if filled with a hatched pattern, output hatch line width and spacing
		if circle.fill_style = HATCHED then
			log (text => text_hatching_line_width & to_string (circle.hatching_line_width) & " "
				& text_hatching_spacing & to_string (circle.hatching_spacing), level => log_threshold);
		end if;
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
		log (text => "assembly doc placeholder face" & to_string (face)
			 & " for " & to_string (placeholder.meaning), level => log_threshold);

		log_indentation_up;
		log (text => text_properties (type_text (placeholder)), level => log_threshold + 1);
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
		log (text => "assembly doc text face" & to_string (face) & latin_1.space
			 & "content '" & to_string (text.content) & "'", level => log_threshold);

		log_indentation_up;
		log (text => text_properties (type_text (text)), level => log_threshold + 1);
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
		log (text => "keepout (courtyard) line face" & to_string (face) & latin_1.space
			 & to_string (type_line_2d (line)), level => log_threshold);
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
		log (text => "keepout (courtyard) arc face" & to_string (face) & latin_1.space 
			 & to_string (type_arc_2d (arc)), level => log_threshold);
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
		log (text => "keepout (courtyard) circle face" & to_string (face) & latin_1.space 
			& to_string (type_circle_2d (circle))
			& " filled" & to_string (circle.filled)
			& " " & text_fill_style & to_string (circle.fill_style), level => log_threshold);
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
		log (text => "stop mask arc face" & to_string (face) & latin_1.space 
			 & to_string (type_arc_2d (arc))
			 & " width" & to_string (arc.width),
			 level => log_threshold);
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
		log (text => "stop mask circle face" & to_string (face) & latin_1.space 
			& to_string (type_circle_2d (circle))
			& " width" & to_string (circle.width)
			& " filled" & to_string (circle.filled)
			& " " & text_fill_style & to_string (circle.fill_style), level => log_threshold);

		-- if filled with a hatched pattern, output hatch line width and spacing
		if circle.fill_style = HATCHED then
			log (text => text_hatching_line_width & to_string (circle.hatching_line_width) & " "
				& text_hatching_spacing & to_string (circle.hatching_spacing), level => log_threshold);
		end if;
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
		log (text => "stop mask line face" & to_string (face) & latin_1.space
			 & to_string (type_line_2d (line))
			 & " width" & to_string (line.width),
			 level => log_threshold);
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
		log (text => "stop mask text face" & to_string (face) & latin_1.space
			 & "content '" & to_string (text.content) & "'", level => log_threshold);

		log_indentation_up;
		log (text => text_properties (type_text (text)), level => log_threshold + 1);
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
		log (text => "solder paste (stencil) arc face" & to_string (face) & latin_1.space 
			 & to_string (type_arc_2d (arc))
			 & " width" & to_string (arc.width),
			 level => log_threshold);
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
		log (text => "solder paste (stencil) circle face" & to_string (face) & latin_1.space 
			& to_string (type_circle_2d (circle))
			& " width" & to_string (circle.width)
			& " filled" & to_string (circle.filled)
			& " " & text_fill_style & to_string (circle.fill_style), level => log_threshold);
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
		log (text => "solder paste (stencil) line face" & to_string (face) & latin_1.space
			 & to_string (type_line_2d (line))
			 & " width" & to_string (line.width),
			 level => log_threshold);
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
		log (text => "route restrict line face" & to_string (face) & latin_1.space
			 & to_string (type_line_2d (line)), level => log_threshold);
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
		log (text => "route restrict arc face" & to_string (face) & latin_1.space 
			 & to_string (type_arc_2d (arc)), level => log_threshold);
	end arc_route_restrict_properties;


	-- CS procedure circle_route_restrict_properties
	

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
		log (text => "via restrict line face" & to_string (face) & latin_1.space
			 & to_string (type_line_2d (line)), level => log_threshold);
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
		log (text => "via restrict arc face" & to_string (face) & latin_1.space 
			 & to_string (type_arc_2d (arc)), level => log_threshold);
	end arc_via_restrict_properties;

	-- CS procedure circle_via_restrict_properties

	
-- PROPERTIES OF OBJECTS IN BOARD CONTOUR / OUTLINE / EDGE CUTS
	procedure line_pcb_contour_properties (
	-- Logs the properties of the given line of pcb contour
		cursor			: in type_pcb_contour_lines.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is
		use type_pcb_contour_lines;
		line : type_pcb_contour_line;
	begin
		line := element (cursor);
		log (text => "PCB contour (edge cuts / outline) line face" & latin_1.space
			 & to_string (type_line_2d (line)), level => log_threshold);
	end line_pcb_contour_properties;

	procedure arc_pcb_contour_properties (
	-- Logs the properties of the given arc of pcb contour
		cursor			: in type_pcb_contour_arcs.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is
		use type_pcb_contour_arcs;
		arc : type_pcb_contour_arc;
	begin
		arc := element (cursor);
		log (text => "PCB contour (edge cuts / outline) arc" & latin_1.space 
			 & to_string (type_arc_2d (arc)), level => log_threshold);
	end arc_pcb_contour_properties;

	procedure circle_pcb_contour_properties (
	-- Logs the properties of the given circle of pcb contour
		cursor			: in type_pcb_contour_circles.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is
		use type_pcb_contour_circles;
		circle : type_pcb_contour_circle;
	begin
		circle := element (cursor);
		log (text => "PCB contour (edge cuts / outline) circle face" & latin_1.space 
			& to_string (type_circle_2d (circle)), level => log_threshold);
	end circle_pcb_contour_properties;


	
	
	
	

	procedure terminal_properties (
	-- Logs the properties of the given terminal.
		terminal		: in et_pcb.type_terminal;
		name			: in et_libraries.type_terminal_name.bounded_string;
		log_threshold 	: in et_string_processing.type_log_level) is
		use et_pcb_coordinates;
		use et_libraries;
		log_threshold_1 : type_log_level := log_threshold + 1;

		use type_pad_lines;
		use type_pad_arcs;
		use type_pad_circles;
		use type_pad_polygons;		
		
		procedure line (cursor : in type_pad_lines.cursor) is begin
			log (text => to_string (type_line_2d (element (cursor))), level => log_threshold + 1);
		end line;

		procedure arc (cursor : in type_pad_arcs.cursor) is begin
			log (text => to_string (type_arc_2d (element (cursor))), level => log_threshold + 1);
		end arc;
		
		procedure circle (cursor : in type_pad_circles.cursor) is begin
			log (text => to_string (type_circle_2d (element (cursor))), level => log_threshold + 1);
		end circle;

		procedure polygon (cursor : in type_pad_polygons.cursor) is 
			use type_polygon_points;
			points : type_polygon_points.set := element (cursor).corners;

			procedure point (cursor : in type_polygon_points.cursor) is begin
				log (text => et_pcb_coordinates.to_string (element (cursor)), level => log_threshold + 1);	
			end point;
	
		begin -- polygon
			log (text => "polygon with corners", level => log_threshold + 1);
			log_indentation_up;
			iterate (points, point'access);
			log_indentation_down;
		end polygon;
	
		
		
	begin -- terminal_properties
		log (text => "terminal name" & to_string (name)
			& " technology" & to_string (terminal.technology)
			& to_string (type_point_2d (terminal.position))
			& to_string (angle => get_angle (terminal.position), preamble => true),
			level => log_threshold);

		log_indentation_up;

		case terminal.technology is
			when THT => 
				
				-- log pad_shape_top/bottom
				log (text => "pad contour top", level => log_threshold + 1);
				iterate (terminal.pad_shape_tht.top.lines, line'access);
				iterate (terminal.pad_shape_tht.top.arcs, arc'access);
				iterate (terminal.pad_shape_tht.top.circles, circle'access);
				iterate (terminal.pad_shape_tht.top.polygons, polygon'access);

				log (text => "pad contour bottom", level => log_threshold + 1);
				iterate (terminal.pad_shape_tht.bottom.lines, line'access);
				iterate (terminal.pad_shape_tht.bottom.arcs, arc'access);
				iterate (terminal.pad_shape_tht.bottom.circles, circle'access);
				iterate (terminal.pad_shape_tht.bottom.polygons, polygon'access);
				
				log (text => "copper width of inner layers" & to_string (terminal.width_inner_layers), level => log_threshold_1);

				case terminal.tht_hole is
					when DRILLED =>
						log (text => "drill" & to_string (terminal.drill_size), level => log_threshold_1); 
					when MILLED =>
						if log_level >= log_threshold_1 then
							log (text => "plated milling contour ");
							log_indentation_up;
								log_plated_millings (terminal.millings, log_threshold_1);
							log_indentation_down;
						end if;
				end case;
				
			when SMT => 
				
				-- log pad_shape
				log (text => "pad contour", level => log_threshold + 1);
				iterate (terminal.pad_shape.lines, line'access);
				iterate (terminal.pad_shape.arcs, arc'access);
				iterate (terminal.pad_shape.circles, circle'access);
				iterate (terminal.pad_shape.polygons, polygon'access);
				
				log (text => "face" & to_string (terminal.face), level => log_threshold_1);
				log (text => "stop mask" & to_string (terminal.stop_mask), level => log_threshold_1);
				log (text => "solder paste" & to_string (terminal.solder_paste), level => log_threshold_1);
		end case;

		log_indentation_down;
	end terminal_properties;


	



	
end et_pcb;

-- Soli Deo Gloria
