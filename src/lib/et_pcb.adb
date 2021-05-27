------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                                PCB                                       --
--                                                                          --
--                              B o d y                                     --
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

with ada.strings.unbounded;
with ada.exceptions;
with ada.tags;

with et_text;					use et_text;

package body et_pcb is

	use pac_shapes;
	


	-- NET CLASSES
	function to_string (net_class_name : in pac_net_class_name.bounded_string) return string is
	begin
		return pac_net_class_name.to_string (net_class_name);
	end to_string;

	function to_net_class_name (net_class_name : in string) return pac_net_class_name.bounded_string is
	begin
		return pac_net_class_name.to_bounded_string (net_class_name);
	end to_net_class_name;
	
	function to_string (class_description : in pac_net_class_description.bounded_string) return string is
	begin
		return pac_net_class_description.to_string (class_description);
	end to_string;

	function to_net_class_description (class_description : in string) return pac_net_class_description.bounded_string is
	begin
		return pac_net_class_description.to_bounded_string (class_description);
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
	
	function to_string (meaning : in type_text_meaning_conductor) return string is begin
		return to_lower (type_text_meaning_conductor'image (meaning));
	end to_string;

	function to_meaning (meaning : in string) return type_text_meaning_conductor is begin
		return type_text_meaning_conductor'value (meaning);
	end to_meaning;


	function on_board (
		point			: in type_point;
		contours		: in type_pcb_contours;
		log_threshold 	: in type_log_level)
		return type_inside_polygon_query_result
	is 
		use pac_pcb_cutouts;
		
		result : type_inside_polygon_query_result;

		procedure query_hole (c : in pac_pcb_cutouts.cursor) is 
			holes : type_inside_polygon_query_result;
		begin
			holes := in_polygon_status (element (c), point);

			-- Merge hole and outline intersections:
			merge_query_results (result, holes);
		end query_hole;
	
	begin -- on_board		
		log (text => "determining position of point" & to_string (point)
			 & " relative to board outline ...", level => log_threshold);

		log_indentation_up;

		-- Starting at given point we imagine a horizontal line that
		-- runs to the right. The line may intersect the board outline 
		-- and the holes:

		-- Get intersections with outline:
		result := in_polygon_status (contours.outline, point);

		-- Get intersections with holes and merge them with result:
		iterate (contours.holes, query_hole'access);		
		
		log (text => "point is " & to_string (result.status), level => log_threshold);
		log_indentation_down;

		return result;
	end on_board;

	
	function on_segment (
		point			: in type_point; -- x/y
		layer			: in type_signal_layer;
		line			: in pac_conductor_lines.cursor;
		accuracy		: in type_catch_zone)
		return boolean 
	is
		result : boolean := false; -- to be returned
		use pac_conductor_lines;
	begin
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
		point			: in type_point; -- x/y
		layer			: in type_signal_layer;
		arc				: in pac_conductor_arcs.cursor;
		accuracy		: in type_distance)
		return boolean 
	is
		result : boolean := false; -- to be returned
		use pac_conductor_arcs;
	begin
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

	

	procedure text_conductor_properties (
		cursor			: in pac_conductor_texts.cursor;
		log_threshold 	: in et_string_processing.type_log_level) 
	is
		use pac_conductor_texts;
		text : type_conductor_text;
	begin
		text := element (cursor);
		log (text => "conductor text signal layer" & to_string (text.layer) & latin_1.space
			& "content '" & et_text.to_string (text.content) & "'", level => log_threshold
			);

		log_indentation_up;
		log (text => text_properties (type_text (text)), level => log_threshold + 1);
		log_indentation_down;
	end text_conductor_properties;

	


-- PROPERTIES OF ELECTRIC OBJECTS IN SIGNAL LAYERS
	procedure route_line_properties (
		cursor			: in pac_conductor_lines.cursor;
		log_threshold 	: in et_string_processing.type_log_level)
	is
		use pac_conductor_lines;
		line : type_conductor_line;
	begin
		line := element (cursor);
		log (text => "segment " & to_string (type_line (line)) &
			 " width" & to_string (line.width) &
			 " layer" & to_string (line.layer)
			 -- CS locked
			 , level => log_threshold);
	end route_line_properties;
	
	procedure route_via_properties (
		cursor			: in pac_vias.cursor;
		log_threshold 	: in et_string_processing.type_log_level) 
	is
		use pac_vias;
		
		procedure do_it (via : type_via) 
		is begin
			case via.category is
				when THROUGH =>
					log (text => "via" 
						& " category " & to_string (via.category) & space
						& to_string (type_drill (via)) 
						& " restring outer" & to_string (via.restring_outer) -- outer layers
						& " restring inner" & to_string (via.restring_inner), -- inner layers
						level => log_threshold);

				when others => null;
				-- CS log properties of other via categories
				
			end case;
						--& " layer_start" & to_string (via.layers.l_start) &
			 --" layer_end" & to_string (via.layers.l_end)
			 ---- CS locked
		end do_it;
	
	begin
		do_it (element (cursor));
	
	end route_via_properties;

		



	procedure pcb_contour_segment_properties (
		cursor			: in pac_polygon_segments.cursor;
		log_threshold 	: in et_string_processing.type_log_level)
	is 
		use pac_polygon_segments;
	begin
		case element (cursor).shape is
			when LINE =>
				log (text => "PCB contour (edge cuts / outline) line" & space
					 & to_string (element (cursor).segment_line),
					 level => log_threshold);

			when ARC =>
				log (text => "PCB contour (edge cuts / outline) arc" & space
					 & to_string (element (cursor).segment_arc),
					 level => log_threshold);
				
		end case;
	end pcb_contour_segment_properties;

	procedure pcb_contour_circle_properties (
		circle			: in type_circle;
		log_threshold 	: in et_string_processing.type_log_level)
	is begin
		log (text => "PCB contour (edge cuts / outline) circle" & space 
			 & to_string (circle),
			 level => log_threshold);
	end pcb_contour_circle_properties;
	

	
end et_pcb;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
