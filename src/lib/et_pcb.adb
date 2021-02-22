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


	function get_dimensions (
		contours		: in type_pcb_contours)
		return type_dimensions
	is
		use et_geometry;
		
		result : type_dimensions;

		procedure update_greatest_x (p : in type_point) is 
			d : type_distance := X (p);
		begin
			if d > X (result.greatest) then
				--put_line ("X" & to_string (d));
				set (axis => X, value => d, point => result.greatest);
			end if;
		end update_greatest_x;
			
		procedure update_greatest_y (p : in type_point) is 
			d : type_distance := Y (p);
		begin
			if d > Y (result.greatest) then
				set (axis => Y, value => d, point => result.greatest);
			end if;
		end update_greatest_y;

		procedure update_smallest_x (p : in type_point) is 
			d : type_distance := X (p);
		begin
			if d < X (result.smallest) then
				set (axis => X, value => d, point => result.smallest);
			end if;
		end update_smallest_x;
			
		procedure update_smallest_y (p : in type_point) is 
			d : type_distance := Y (p);
		begin
			if d < Y (result.smallest) then
				set (axis => Y, value => d, point => result.smallest);
			end if;
		end update_smallest_y;

		
		use pac_pcb_contour_lines;
		use pac_pcb_contour_arcs;
		--use pac_pcb_contour_circles;

		procedure query_line (c : in pac_pcb_contour_lines.cursor) is 
		begin
			update_greatest_x (element (c).start_point);
			update_greatest_y (element (c).start_point);
			update_smallest_x (element (c).start_point);
			update_smallest_y (element (c).start_point);

			update_greatest_x (element (c).end_point);
			update_greatest_y (element (c).end_point);
			update_smallest_x (element (c).end_point);
			update_smallest_y (element (c).end_point);
		end query_line;
		
	begin
		iterate (contours.lines, query_line'access);
		-- CS arcs, circles
		
		return result;
	end get_dimensions;
	
	function on_board (
		point			: in type_point;
		contours		: in type_pcb_contours;
		log_threshold 	: in type_log_level)
		return boolean
	is 
		-- The approach to detect whether the given point is inside or outside the
		-- board area is as follows:
		-- 1. Build a probe line (starting at point) that runs at zero degrees
		--    to the right.
		-- 2. The number of intersections then tells us:
		--    - odd -> point is inside board area
		--    - zero or even -> point is outside board area

		result : boolean := false;
		

		type type_line is new pac_shapes.type_line with null record;
		line : constant type_line := (
				start_point	=> point,
				end_point	=> type_point (set (X (point) + 1.0, Y (point))));
		
		probe_line : constant type_line_vector := to_line_vector (line);

		subtype type_intersections_total is natural range 0 .. 1000; -- CS
		it : type_intersections_total := 0;

		procedure increment_intersections is begin
			it := it + 1;
		end increment_intersections;
		
		-- This procedure iterates lines, arcs and circles of the given
		-- contours and counts the intersections of the ray with each of them:
		procedure count_intersections is 

			use pac_pcb_contour_lines;
			
			procedure query_line (c : in pac_pcb_contour_lines.cursor) is
				i : type_intersection := get_intersection (probe_line, element (c));
			begin				
				if i.status = EXISTS then

					if X (to_point (i.intersection)) > X (point) then
					
						log (text => "intersects line" & to_string (element (c))
							& " at" & to_string (to_point (i.intersection)),
							level => log_threshold + 2);
						
						increment_intersections;
					end if;
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

		log (text => "using a probe line: start" 
			 & to_string (to_point (probe_line.v_start))
			 & " direction 0 degrees ...", 
			 level => log_threshold + 1);
			
		log (text => "counting intersections of probe line with board outline ...",
			 level => log_threshold + 1);

		log_indentation_up;
		
		-- Count the intersections of the probe line with the
		-- segments of the board countour:
		count_intersections;

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
