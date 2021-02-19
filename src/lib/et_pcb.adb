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
		return boolean
	is 
		-- The approach to detect whether the given point is inside or outside the
		-- board area is as follows:
		-- 1. Build a ray (starting at point) that travels toward the 
		--    board area. The ray can be regarded as a shot through the board
		--    so that it intersects at least two or more segments of the contour.
		-- 2. The number of intersections then tells us:
		--    - odd number of intersections -> point is inside board area
		--    - even number -> point is outside board area

		result : boolean := false;
		
		use pac_pcb_contour_lines;
		line_cursor : pac_pcb_contour_lines.cursor := contours.lines.first;
		cp : type_point;

		-- This ray starts at the given point and runs toward the
		-- polygon. Its direction changes in the course of a probing
		-- mechanism:
		ray : type_ray := (start_point => point, direction => zero_rotation);

		subtype type_intersections_total is natural range 0 .. 1000; -- CS
		it : type_intersections_total := 0;

		procedure increment_intersections is begin
			it := it + 1;
		end increment_intersections;
		
		-- This procedure iterates lines, arcs and circles of the given
		-- contours and counts the intersections of the ray with each of them:
		procedure count_intersections is 
			procedure query_line (c : in pac_pcb_contour_lines.cursor) is
				i : type_intersection := get_intersection (ray, element (c));
			begin				
				if i.status = EXISTS then
					
					log (text => "line" & to_string (element (c))
						 & " at" & to_string (to_point (i.intersection)),
						 level => log_threshold + 2);
					
					increment_intersections;
				end if;
			end query_line;

		begin
			iterate (contours.lines, query_line'access);

			-- CS arcs and lines
		end count_intersections;
		
	begin
		log (text => "determining position of point" & to_string (point)
			 & " relative to board outline ...", level => log_threshold);

		log_indentation_up;

		
		-- It is sufficient to make the ray travel in the direction of just one segment.
		
		-- We look for a suitable line:
		log (text => "searching a suitable segment ...", level => log_threshold + 1);
		log_indentation_up;
		
		while line_cursor /= pac_pcb_contour_lines.no_element loop
			-- Get the center of the contour line:
			cp := get_center (element (line_cursor));

			-- The contour line is suitable if its center is different from
			-- the given point. Point and cp must not be equal.
			if point /= cp then
				
				-- Set the direction required for the ray so that
				-- the ray intersects the contour line at its center:
				ray.direction := angle (distance_polar (point, cp));

				-- The contour line is suitable if it does not run 
				-- in the same direction as the ray. In that case this 
				-- loop is to be cancelled.
				if direction (element (line_cursor)) /= ray.direction then

					log (text => "selected line:" & to_string (element (line_cursor)),
						 level => log_threshold + 2);
					
					log (text => "scan ray starts at" & to_string (ray.start_point) 
						 & " in direction" & to_string (ray.direction),
						 level => log_threshold + 2);
					
					log (text => "ray intersects line at" & to_string (cp),
						 level => log_threshold + 2);
					
					exit;
				end if;
				
			end if;
			
			next (line_cursor);
		end loop;

		log_indentation_down;

		log (text => "counting intersections of ray with board outline ...",
			 level => log_threshold + 1);

		log_indentation_up;
		
		-- If a suitable line has been found then count the remaining
		-- intersections of the ray with other segments of the countour:
		if line_cursor /= pac_pcb_contour_lines.no_element then
			count_intersections;
		else
			-- -- CS search among the arcs, circles ...
			null;
		end if;

		log_indentation_down;
		
		log (text => "intersections total:" & positive'image (it), level => log_threshold + 1);

		log_indentation_down;

		-- If the total iterations is an odd number, then the given point
		-- is on the board.
		-- If the total is even, then the point is outside the board area.
		if (it rem 2) = 1 then
			log (text => "point IS on board", level => log_threshold);
			result := true; -- inside
		else 
			log (text => "point is NOT on board", level => log_threshold);
			result := false; -- outside
		end if;

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

		

-- PROPERTIES OF OBJECTS IN BOARD CONTOUR / OUTLINE / EDGE CUTS
	procedure line_pcb_contour_properties (
	-- Logs the properties of the given line of pcb contour
		cursor			: in pac_pcb_contour_lines.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is
		use pac_pcb_contour_lines;
		line : type_pcb_contour_line;
	begin
		line := element (cursor);
		log (text => "PCB contour (edge cuts / outline) line" & latin_1.space
			 & to_string (type_line (line)), level => log_threshold);
			-- CS lock status
	end line_pcb_contour_properties;

	procedure arc_pcb_contour_properties (
	-- Logs the properties of the given arc of pcb contour
		cursor			: in pac_pcb_contour_arcs.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is
		use pac_pcb_contour_arcs;
		arc : type_pcb_contour_arc;
	begin
		arc := element (cursor);
		log (text => "PCB contour (edge cuts / outline) arc" & latin_1.space 
			 & to_string (type_arc (arc)), level => log_threshold);
			-- CS lock status
	end arc_pcb_contour_properties;

	procedure circle_pcb_contour_properties (
	-- Logs the properties of the given circle of pcb contour
		cursor			: in pac_pcb_contour_circles.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is
		use pac_pcb_contour_circles;
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
