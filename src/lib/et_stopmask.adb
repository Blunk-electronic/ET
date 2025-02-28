------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                          SOLDER STOPMASK                                 --
--                                                                          --
--                              B o d y                                     --
--                                                                          --
-- Copyright (C) 2017 - 2025                                                --
-- Mario Blunk / Blunk electronic                                           --
-- Buchfinkenweg 3 / 99097 Erfurt / Germany                                 --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
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
--   to do:

package body et_stopmask is

	
	procedure iterate (
		lines	: in pac_stop_lines.list;
		process	: not null access procedure (position : in pac_stop_lines.cursor);
		proceed	: not null access boolean)
	is
		c : pac_stop_lines.cursor := lines.first;
	begin
		while c /= pac_stop_lines.no_element and proceed.all = TRUE loop
			process (c);
			next (c);
		end loop;
	end iterate;



	
	function is_proposed (
		line_cursor	: in pac_stop_lines.cursor)
		return boolean
	is begin
		if is_proposed (element (line_cursor)) then
			return true;
		else
			return false;
		end if;
	end is_proposed;

	

	function is_selected (
		line_cursor	: in pac_stop_lines.cursor)
		return boolean
	is begin
		if is_selected (element (line_cursor)) then
			return true;
		else
			return false;
		end if;
	end is_selected;


	
	procedure mirror_lines (
		lines	: in out pac_stop_lines.list;
		axis	: in type_mirror := MIRROR_ALONG_Y_AXIS)
	is
		result : pac_stop_lines.list;

		procedure query_line (c : in pac_stop_lines.cursor) is
			line : type_stop_line := element (c);
		begin
			mirror (line, axis);
			result.append (line);
		end query_line;
		
	begin
		lines.iterate (query_line'access);
		lines := result;
	end mirror_lines;


	
	procedure rotate_lines (
		lines	: in out pac_stop_lines.list;
		angle	: in type_rotation_model)
	is
		result : pac_stop_lines.list;

		procedure query_line (c : in pac_stop_lines.cursor) is
			line : type_stop_line := element (c);
		begin
			rotate_by (line, angle);
			result.append (line);
		end query_line;

	begin
		lines.iterate (query_line'access);
		lines := result;
	end rotate_lines;


	
	procedure move_lines (
		lines	: in out pac_stop_lines.list;
		offset	: in type_distance_relative)
	is
		result : pac_stop_lines.list;

		procedure query_line (c : in pac_stop_lines.cursor) is
			line : type_stop_line := element (c);
		begin
			move_by (line, offset);
			result.append (line);
		end query_line;

	begin
		lines.iterate (query_line'access);
		lines := result;
	end move_lines;





	function is_proposed (
		arc_cursor	: in pac_stop_arcs.cursor)
		return boolean
	is begin
		-- if element (arc_cursor).status.proposed then
		-- 	return true;
		-- else
		-- 	return false;
		-- end if;

		return false; -- CS
	end is_proposed;
	

	function is_selected (
		arc_cursor	: in pac_stop_arcs.cursor)
		return boolean
	is begin
		-- if element (arc_cursor).status.proposed then
		-- 	return true;
		-- else
		-- 	return false;
		-- end if;

		return false; -- CS
	end is_selected;


	
	
	procedure mirror_arcs (
		arcs	: in out pac_stop_arcs.list;
		axis	: in type_mirror := MIRROR_ALONG_Y_AXIS)
	is
		result : pac_stop_arcs.list;

		procedure query_arc (c : in pac_stop_arcs.cursor) is
			arc : type_stop_arc := element (c);
		begin
			mirror (arc, axis);
			result.append (arc);
		end query_arc;
		
	begin
		arcs.iterate (query_arc'access);
		arcs := result;
	end mirror_arcs;

	

	procedure rotate_arcs (
		arcs	: in out pac_stop_arcs.list;
		angle	: in type_rotation_model)
	is
		result : pac_stop_arcs.list;

		procedure query_arc (c : in pac_stop_arcs.cursor) is
			arc : type_stop_arc := element (c);
		begin
			rotate_by (arc, angle);
			result.append (arc);
		end query_arc;

	begin
		arcs.iterate (query_arc'access);
		arcs := result;
	end rotate_arcs;


	
	procedure move_arcs (
		arcs	: in out pac_stop_arcs.list;
		offset	: in type_distance_relative)		
	is
		result : pac_stop_arcs.list;

		procedure query_arc (c : in pac_stop_arcs.cursor) is
			arc : type_stop_arc := element (c);
		begin
			move_by (arc, offset);
			result.append (arc);
		end query_arc;

	begin
		arcs.iterate (query_arc'access);
		arcs := result;
	end move_arcs;





	function is_proposed (
		circle_cursor	: in pac_stop_circles.cursor)
		return boolean
	is begin
		-- if element (circle_cursor).status.proposed then
		-- 	return true;
		-- else
		-- 	return false;
		-- end if;

		return false; -- CS
	end is_proposed;
	

	function is_selected (
		circle_cursor	: in pac_stop_circles.cursor)
		return boolean
	is begin
		-- if element (circle_cursor).status.proposed then
		-- 	return true;
		-- else
		-- 	return false;
		-- end if;

		return false; -- CS
	end is_selected;


	

	procedure mirror_circles (
		circles	: in out pac_stop_circles.list;
		axis	: in type_mirror := MIRROR_ALONG_Y_AXIS)
	is
		result : pac_stop_circles.list;

		procedure query_circle (c : in pac_stop_circles.cursor) is
			circle : type_stop_circle := element (c);
		begin
			mirror (circle, axis);
			result.append (circle);
		end query_circle;
		
	begin
		circles.iterate (query_circle'access);
		circles := result;
	end mirror_circles;


	
	procedure rotate_circles (
		circles	: in out pac_stop_circles.list;
		angle	: in type_rotation_model)		
	is
		result : pac_stop_circles.list;

		procedure query_circle (c : in pac_stop_circles.cursor) is
			circle : type_stop_circle := element (c);
		begin
			rotate_by (circle, angle);
			result.append (circle);
		end query_circle;

	begin
		circles.iterate (query_circle'access);		
		circles := result;
	end rotate_circles;			


	
	procedure move_circles (
		circles	: in out pac_stop_circles.list;
		offset	: in type_distance_relative)		
	is
		result : pac_stop_circles.list;

		procedure query_circle (c : in pac_stop_circles.cursor) is
			circle : type_stop_circle := element (c);
		begin
			move_by (circle, offset);
			result.append (circle);
		end query_circle;

	begin
		circles.iterate (query_circle'access);
		circles := result;
	end move_circles;




	procedure iterate (
		zones	: in pac_stop_zones.list;
		process	: not null access procedure (position : in pac_stop_zones.cursor);
		proceed	: not null access boolean)
	is
		c : pac_stop_zones.cursor := zones.first;
	begin
		while c /= pac_stop_zones.no_element and proceed.all = TRUE loop
			process (c);
			next (c);
		end loop;
	end iterate;
	
	

	procedure mirror_contours (
		contours	: in out pac_stop_zones.list;
		axis	: in type_mirror := MIRROR_ALONG_Y_AXIS)
	is
		result : pac_stop_zones.list;

		procedure query_contour (c : in pac_stop_zones.cursor) is
			contour : type_stop_zone := element (c);
		begin
			mirror (contour, axis);
			result.append (contour);
		end query_contour;
		
	begin
		contours.iterate (query_contour'access);
		contours := result;
	end mirror_contours;

	

	procedure rotate_contours (
		contours	: in out pac_stop_zones.list;
		angle		: in type_rotation_model)		
	is
		result : pac_stop_zones.list;

		procedure query_contour (c : in pac_stop_zones.cursor) is
			contour : type_stop_zone := element (c);
		begin
			rotate_by (contour, angle);
			result.append (contour);
		end query_contour;

	begin
		contours.iterate (query_contour'access);		
		contours := result;
	end rotate_contours;			


	
	procedure move_contours (
		contours	: in out pac_stop_zones.list;
		offset		: in type_distance_relative)		
	is
		result : pac_stop_zones.list;

		procedure query_contour (c : in pac_stop_zones.cursor) is
			contour : type_stop_zone := element (c);
		begin
			move_by (contour, offset);
			result.append (contour);
		end query_contour;

	begin
		contours.iterate (query_contour'access);
		contours := result;
	end move_contours;






	function is_proposed (
		text_cursor	: in pac_stop_texts.cursor)
		return boolean
	is begin
		if is_proposed (element (text_cursor)) then
			return true;
		else
			return false;
		end if;
	end is_proposed;

	

	function is_selected (
		text_cursor	: in pac_stop_texts.cursor)
		return boolean
	is begin
		if is_selected (element (text_cursor)) then
			return true;
		else
			return false;
		end if;
	end is_selected;
		


	

	function to_string (
		text : in pac_stop_texts.cursor)
		return string
	is begin
		return to_string (element (text));
	end to_string;
	


	

	procedure iterate (
		texts	: in pac_stop_texts.list;
		process	: not null access procedure (position : in pac_stop_texts.cursor);
		proceed	: not null access boolean)
	is
		c : pac_stop_texts.cursor := texts.first;
	begin
		while c /= pac_stop_texts.no_element and proceed.all = TRUE loop
			process (c);
			next (c);
		end loop;
	end iterate;




	

	
	procedure mirror_texts (
		texts	: in out pac_stop_texts.list;
		axis	: in type_mirror := MIRROR_ALONG_Y_AXIS)
	is
		result : pac_stop_texts.list;

		procedure query_text (c : in pac_stop_texts.cursor) is
			text : type_stop_text := element (c);
		begin
			result.append (text);
		end query_text;
		
	begin
		texts.iterate (query_text'access);		
		texts := result;
	end mirror_texts;


	
	procedure rotate_texts (
		texts	: in out pac_stop_texts.list;
		angle	: in type_rotation_model)
	is
		result : pac_stop_texts.list;

		procedure query_text (c : in pac_stop_texts.cursor) is
			text : type_stop_text := element (c);
		begin
			result.append (text);
		end query_text;
		
	begin
		texts.iterate (query_text'access);		
		texts := result;
	end rotate_texts;


	
	procedure move_texts (
		texts	: in out pac_stop_texts.list;
		offset	: in type_distance_relative)
	is
		result : pac_stop_texts.list;

		procedure query_text (c : in pac_stop_texts.cursor) is
			text : type_stop_text := element (c);
		begin
			result.append (text);
		end query_text;
		
	begin
		texts.iterate (query_text'access);		
		texts := result;
	end move_texts;
	

	
	procedure arc_stop_mask_properties (
		face			: in type_face;
		cursor			: in pac_stop_arcs.cursor;
		log_threshold 	: in type_log_level) 
	is
		use pac_stop_arcs;
		arc : type_stop_arc;
	begin
		arc := element (cursor);
		log (text => "stop mask arc face" & to_string (face) & space 
			 & to_string (type_arc (arc))
			 & " width" & to_string (arc.width),
			 level => log_threshold);
	end arc_stop_mask_properties;


	
	procedure circle_stop_mask_properties (
		face			: in type_face;
		cursor			: in pac_stop_circles.cursor;
		log_threshold 	: in type_log_level) 
	is
		use pac_stop_circles;
	begin
		log (text => "stop mask circle face" & to_string (face) & space 
			& to_string (element (cursor)),
			level => log_threshold);
	end;


	
	procedure line_stop_mask_properties (
		face			: in type_face;
		cursor			: in pac_stop_lines.cursor;
		log_threshold 	: in type_log_level) 
	is
		use pac_stop_lines;
		line : type_stop_line;
	begin
		line := element (cursor);
		log (text => "stop mask line face" & to_string (face) & space
			 & to_string (type_line (line))
			 & " width" & to_string (line.width),
			 level => log_threshold);
	end line_stop_mask_properties;

	

	procedure text_stop_mask_properties (
		face			: in type_face;
		cursor			: in pac_stop_texts.cursor;
		log_threshold 	: in type_log_level) 
	is
		use et_text;
		text : type_stop_text;
	begin
		text := element (cursor);
		log (text => "stop mask text face" & to_string (face) & space
			 & "content '" & to_string (text.content) & "'", level => log_threshold);

		log_indentation_up;
		-- CS log (text => text_properties (type_text (text)), level => log_threshold + 1);
		log_indentation_down;
	end text_stop_mask_properties;

	
end et_stopmask;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
