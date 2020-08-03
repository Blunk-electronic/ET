------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                                PCB                                       --
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

with et_packages;
with et_string_processing;		use et_string_processing;
with et_text;					use et_text;

package body et_pcb is
	use pac_shapes;

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
	
	function text_properties (text : in et_packages.type_text) return string is
	-- Returns the properties of the given text in a long single string.
	begin
		return to_string (text.position) & latin_1.space
			& "size" 
			& to_string (text.size)
			& " line width" & to_string (text.line_width)
			& " rotation" & to_string (rot (text.position))
			& et_text.to_string (text.alignment);
	end text_properties;
	
	function to_string (meaning : in type_text_meaning_copper) return string is begin
		return to_lower (type_text_meaning_copper'image (meaning));
	end to_string;

	function to_meaning (meaning : in string) return type_text_meaning_copper is begin
		return type_text_meaning_copper'value (meaning);
	end to_meaning;
	
	function on_segment (
	-- Returns true if the given point sits on the given line of copper.
		point			: in type_point; -- x/y
		layer			: in type_signal_layer;
		line			: in pac_copper_lines.cursor;
		accuracy		: in type_catch_zone)
		return boolean is
		result : boolean := false; -- to be returned
		use pac_copper_lines;
	begin -- on_segment
		if element (line).layer = layer then
			if on_line (point, element (line), accuracy) then
				result := true;
			else
				result := false;
			end if;
		else
			result := false;
		end if;
		
		return result;
	end on_segment;

	function on_segment (
	-- Returns true if the given point sits on the given arc of copper.
		point			: in type_point; -- x/y
		layer			: in type_signal_layer;
		arc				: in pac_copper_arcs.cursor;
		accuracy		: in type_distance)
		return boolean is
		result : boolean := false; -- to be returned
		use pac_copper_arcs;
	begin -- on_segment
		if element (arc).layer = layer then
			result := true; -- CS
		else
			result := false;
		end if;

		return result;
	end on_segment;


	function to_string (locked : in type_locked) return string is begin
		return to_lower (type_locked'image (locked));
	end;

	function to_lock_status (locked : in string) return type_locked is begin
		return type_locked'value (locked);
	end;

	function to_string (priority_level : in type_polygon_priority) return string is begin
		return type_polygon_priority'image (priority_level);
	end;

	function to_polygon_priority (priority_level : in string) return type_polygon_priority is begin
		return type_polygon_priority'value (priority_level);
	end;


	function to_string (polygon_pad_connection : in type_polygon_pad_connection) return string is begin
		return to_lower (type_polygon_pad_connection'image (polygon_pad_connection));
	end;

	function to_pad_connection (connection : in string) return type_polygon_pad_connection is begin
		return type_polygon_pad_connection'value (connection);
	end;
	

	function to_string (polygon_pad_technology : in type_polygon_pad_technology) return string is begin
		return to_lower (type_polygon_pad_technology'image (polygon_pad_technology));
	end;

	function to_pad_technology (technology : in string) return type_polygon_pad_technology is begin
		return type_polygon_pad_technology'value (technology);
	end to_pad_technology;

	

	function package_position (position : in type_package_position) return string is
	-- Returns the coordinates of a package (in a board) as string.
	begin
		return (" position" & to_string (type_point (position))
			& " angle" & to_string (rot (position))
			& " face" & to_string (get_face (position)));
	end package_position;

	
	function to_string (flipped : in type_flipped) return string is begin
		return to_lower (type_flipped'image (flipped));
	end;

	function to_flipped (flipped : in string) return type_flipped is begin
		return type_flipped'value (flipped);
	end;

	

	procedure text_copper_properties (
	-- Logs the properties of the given text of copper
		cursor			: in pac_texts.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is
		use pac_texts;
		text : type_text;
	begin
		text := element (cursor);
		log (text => "copper text signal layer" & to_string (text.layer) & latin_1.space
			& "content '" & et_text.to_string (text.content) & "'", level => log_threshold
			);

		log_indentation_up;
		log (text => text_properties (type_text (text)), level => log_threshold + 1);
		log_indentation_down;
	end text_copper_properties;

	


-- PROPERTIES OF ELECTRIC OBJECTS IN SIGNAL LAYERS
	procedure route_line_properties (
	-- Logs the properties of the given line of a route
		cursor			: in pac_copper_lines.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is
		use pac_copper_lines;
		line : type_copper_line;
	begin
		line := element (cursor);
		log (text => "segment " & to_string (type_line (line)) &
			 " width" & to_string (line.width) &
			 " layer" & to_string (line.layer)
			 -- CS locked
			 , level => log_threshold);
	end route_line_properties;
	
	procedure route_via_properties (
	-- Logs the properties of the given via of a route
		cursor			: in pac_vias.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is
		use pac_vias;
		via : type_via;
	begin
		via := element (cursor);
		log (text => "via" & to_string (type_drill (via)) &
			 " restring_outer" & to_string (via.restring_outer) & -- outer layers
			 " restring_inner" & to_string (via.restring_inner) & -- inner layers
			 " layer_start" & to_string (via.layer_start) &
			 " layer_end" & to_string (via.layer_end)
			 -- CS locked
			 , level => log_threshold);
	end route_via_properties;

-- 	procedure route_polygon_properties (
-- 	-- Logs the properties of the given polygon of a route
-- 		cursor			: in pac_copper_polygons_signal.cursor;
-- 		log_threshold 	: in et_string_processing.type_log_level) is
-- 		use pac_copper_polygons_signal;
-- 		use type_polygon_points;
-- 		points : type_polygon_points.set;
-- 		point_cursor : type_polygon_points.cursor;
-- 	begin
-- 		-- general stuff
-- 		log (text => "polygon" & 
-- 			 " " & text_polygon_signal_layer & to_string (element (cursor).layer) &
-- 			 " " & text_polygon_width_min & to_string (element (cursor).width_min) &
-- 			 " " & text_polygon_pad_connection & to_string (element (cursor).pad_connection) &
-- 			 " " & text_polygon_priority_level & to_string (element (cursor).priority_level) &
-- 			 " " & text_polygon_isolation_gap & to_string (element (cursor).isolation_gap) &
-- 			 " " & text_polygon_corner_easing & to_string (element (cursor).corner_easing) &
-- 			 " " & text_polygon_easing_radius & to_string (element (cursor).easing_radius),
-- 			 level => log_threshold);
-- 
-- 		log_indentation_up;
-- 		
-- 		-- type depended stuff
-- 		case element (cursor).pad_connection is
-- 			when THERMAL =>
-- 				log (text => text_polygon_pad_technology & to_string (element (cursor).thermal_technology) &
-- 					" " & text_polygon_thermal_width & to_string (element (cursor).thermal_width) &
-- 					" " & text_polygon_thermal_gap & to_string (element (cursor).thermal_gap),
-- 					level => log_threshold);
-- 
-- 			when SOLID =>
-- 				log (text => text_polygon_pad_technology & to_string (element (cursor).solid_technology),
-- 					level => log_threshold);
-- 				
-- 			when NONE =>
-- 				null;
-- 		end case;
-- 
-- 		-- corner points
-- 		log (text => text_polygon_corner_points, level => log_threshold);
-- 		points := element (cursor).corners;
-- 		point_cursor := points.first;
-- 		while point_cursor /= type_polygon_points.no_element loop
-- 			log (text => to_string (element (point_cursor)), level => log_threshold);
-- 			next (point_cursor);
-- 		end loop;
-- 		
-- 		log_indentation_down;
-- 	end route_polygon_properties;


-- PROPERTIES OF OBJECTS IN BOARD CONTOUR / OUTLINE / EDGE CUTS
	procedure line_pcb_contour_properties (
	-- Logs the properties of the given line of pcb contour
		cursor			: in type_pcb_contour_lines.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is
		use type_pcb_contour_lines;
		line : type_pcb_contour_line;
	begin
		line := element (cursor);
		log (text => "PCB contour (edge cuts / outline) line" & latin_1.space
			 & to_string (type_line (line)), level => log_threshold);
			-- CS lock status
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
			 & to_string (type_arc (arc)), level => log_threshold);
			-- CS lock status
	end arc_pcb_contour_properties;

	procedure circle_pcb_contour_properties (
	-- Logs the properties of the given circle of pcb contour
		cursor			: in type_pcb_contour_circles.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is
		use type_pcb_contour_circles;
		circle : type_pcb_contour_circle;
	begin
		circle := element (cursor);
		log (text => "PCB contour (edge cuts / outline) circle" & latin_1.space 
			 & to_string (type_circle (circle)), level => log_threshold);
			-- CS lock status
	end circle_pcb_contour_properties;


	
end et_pcb;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
