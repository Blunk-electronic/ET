------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                     STENCIL / SOLDER PASTE MASK                          --
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

with ada.strings;				use ada.strings;

package body et_stencil is

-- LINES

	procedure iterate (
		lines	: in pac_stencil_lines.list;
		process	: not null access procedure (position : in pac_stencil_lines.cursor);
		proceed	: not null access boolean)
	is
		c : pac_stencil_lines.cursor := lines.first;
	begin
		while c /= pac_stencil_lines.no_element and proceed.all = TRUE loop
			process (c);
			next (c);
		end loop;
	end iterate;


	
	function is_proposed (
		line_cursor	: in pac_stencil_lines.cursor)
		return boolean
	is begin
		if is_proposed (element (line_cursor)) then
			return true;
		else
			return false;
		end if;
	end is_proposed;

	

	function is_selected (
		line_cursor	: in pac_stencil_lines.cursor)
		return boolean
	is begin
		if is_selected (element (line_cursor)) then
			return true;
		else
			return false;
		end if;
	end is_selected;


	
	procedure mirror_lines (
		lines	: in out pac_stencil_lines.list;
		axis	: in type_mirror := MIRROR_ALONG_Y_AXIS)
	is
		result : pac_stencil_lines.list;

		procedure query_line (c : in pac_stencil_lines.cursor) is
			line : type_stencil_line := element (c);
		begin
			mirror (line, axis);
			result.append (line);
		end query_line;
		
	begin
		lines.iterate (query_line'access);
		lines := result;
	end mirror_lines;

	
	
	procedure rotate_lines (
		lines	: in out pac_stencil_lines.list;
		angle	: in type_rotation_model)
	is
		result : pac_stencil_lines.list;

		procedure query_line (c : in pac_stencil_lines.cursor) is
			line : type_stencil_line := element (c);
		begin
			rotate_by (line, angle);
			result.append (line);
		end query_line;
		
	begin
		lines.iterate (query_line'access);
		lines := result;
	end rotate_lines;

	
	
	procedure move_lines (
		lines	: in out pac_stencil_lines.list;
		offset	: in type_distance_relative)
	is
		result : pac_stencil_lines.list;

		procedure query_line (c : in pac_stencil_lines.cursor) is
			line : type_stencil_line := element (c);
		begin
			move_by (line, offset);
			result.append (line);
		end query_line;
		
	begin
		lines.iterate (query_line'access);
		lines := result;
	end move_lines;	



	
	
-- ARCS

	function is_proposed (
		arc_cursor	: in pac_stencil_arcs.cursor)
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
		arc_cursor	: in pac_stencil_arcs.cursor)
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
		arcs	: in out pac_stencil_arcs.list;
		axis	: in type_mirror := MIRROR_ALONG_Y_AXIS)
	is
		result : pac_stencil_arcs.list;

		procedure query_arc (c : in pac_stencil_arcs.cursor) is
			arc : type_stencil_arc := element (c);
		begin
			mirror (arc, axis);
			result.append (arc);
		end query_arc;
		
	begin
		arcs.iterate (query_arc'access);
		arcs := result;
	end mirror_arcs;

	
	
	procedure rotate_arcs (
		arcs	: in out pac_stencil_arcs.list;
		angle	: in type_rotation_model)
	is
		result : pac_stencil_arcs.list;

		procedure query_arc (c : in pac_stencil_arcs.cursor) is
			arc : type_stencil_arc := element (c);
		begin
			rotate_by (arc, angle);
			result.append (arc);
		end query_arc;
		
	begin
		arcs.iterate (query_arc'access);
		arcs := result;
	end rotate_arcs;

	
	
	procedure move_arcs (
		arcs	: in out pac_stencil_arcs.list;
		offset	: in type_distance_relative)
	is
		result : pac_stencil_arcs.list;

		procedure query_arc (c : in pac_stencil_arcs.cursor) is
			arc : type_stencil_arc := element (c);
		begin
			move_by (arc, offset);
			result.append (arc);
		end query_arc;
		
	begin
		arcs.iterate (query_arc'access);
		arcs := result;
	end move_arcs;



	
	
-- CIRCLES


	function is_proposed (
		circle_cursor	: in pac_stencil_circles.cursor)
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
		circle_cursor	: in pac_stencil_circles.cursor)
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
		circles	: in out pac_stencil_circles.list;
		axis	: in type_mirror := MIRROR_ALONG_Y_AXIS)
	is
		result : pac_stencil_circles.list;

		procedure query_circle (c : in pac_stencil_circles.cursor) is
			circle : type_stencil_circle := element (c);
		begin
			mirror (circle, axis);
			result.append (circle);
		end query_circle;
		
	begin
		circles.iterate (query_circle'access);
		circles := result;
	end mirror_circles;
	

	
	procedure rotate_circles (
		circles	: in out pac_stencil_circles.list;
		angle	: in type_rotation_model)
	is
		result : pac_stencil_circles.list;

		procedure query_circle (c : in pac_stencil_circles.cursor) is
			circle : type_stencil_circle := element (c);
		begin
			rotate_by (circle, angle);
			result.append (circle);
		end query_circle;

	begin
		circles.iterate (query_circle'access);
		circles := result;
	end rotate_circles;

	
	
	procedure move_circles (
		circles	: in out pac_stencil_circles.list;
		offset	: in type_distance_relative)
	is
		result : pac_stencil_circles.list;

		procedure query_circle (c : in pac_stencil_circles.cursor) is
			circle : type_stencil_circle := element (c);
		begin
			move_by (circle, offset);
			result.append (circle);
		end query_circle;
		
	begin
		circles.iterate (query_circle'access);
		circles := result;
	end move_circles;



	
	
-- CONTOURS


	procedure iterate (
		zones	: in pac_stencil_zones.list;
		process	: not null access procedure (position : in pac_stencil_zones.cursor);
		proceed	: not null access boolean)
	is
		c : pac_stencil_zones.cursor := zones.first;
	begin
		while c /= pac_stencil_zones.no_element and proceed.all = TRUE loop
			process (c);
			next (c);
		end loop;
	end iterate;

	


	function get_first_open (
		zones : in out pac_stencil_zones.list)
		return pac_stencil_zones.cursor
	is
		result : pac_stencil_zones.cursor;
		proceed : aliased boolean := true;

		procedure query_contour (c : in pac_stencil_zones.cursor) is
			status : type_contour_status := is_closed (element (c));
		begin
			if not status.closed then
				proceed := false;
				result := c;
			end if;
		end query_contour;
			
	begin		
		iterate (zones, query_contour'access, proceed'access);
		return result;
	end get_first_open;

	


	function get_open_zones (
		zones : in out pac_stencil_zones.list)
		return pac_stencil_zone_cursors.list
	is
		result : pac_stencil_zone_cursors.list;

		procedure query_zone (c : in pac_stencil_zones.cursor) is
			status : type_contour_status := is_closed (element (c));
		begin
			if not status.closed then
				result.append (c);
			end if;			
		end query_zone;
		
	begin
		zones.iterate (query_zone'access);
		return result;
	end get_open_zones;


	
	
	
	procedure mirror_contours (
		contours	: in out pac_stencil_zones.list;
		axis		: in type_mirror := MIRROR_ALONG_Y_AXIS)
	is
		result : pac_stencil_zones.list;

		procedure query_contour (c : in pac_stencil_zones.cursor) is
			contour : type_stencil_zone := element (c);
		begin
			mirror (contour, axis);
			result.append (contour);
		end query_contour;
		
	begin
		contours.iterate (query_contour'access);
		contours := result;
	end mirror_contours;
	

	procedure rotate_contours (
		contours	: in out pac_stencil_zones.list;
		angle		: in type_rotation_model)
	is
		result : pac_stencil_zones.list;

		procedure query_contour (c : in pac_stencil_zones.cursor) is
			contour : type_stencil_zone := element (c);
		begin
			rotate_by (contour, angle);
			result.append (contour);
		end query_contour;

	begin
		contours.iterate (query_contour'access);
		contours := result;
	end rotate_contours;

	
	procedure move_contours (
		contours	: in out pac_stencil_zones.list;
		offset		: in type_distance_relative)
	is
		result : pac_stencil_zones.list;

		procedure query_contour (c : in pac_stencil_zones.cursor) is
			contour : type_stencil_zone := element (c);
		begin
			move_by (contour, offset);
			result.append (contour);
		end query_contour;

	begin
		contours.iterate (query_contour'access);
		contours := result;
	end move_contours;



	
	
	procedure mirror_stencil_objects (
		stencil	: in out type_stencil;
		axis	: in type_mirror := MIRROR_ALONG_Y_AXIS)
	is begin
		mirror_lines (stencil.lines);
		mirror_arcs (stencil.arcs);
		mirror_circles (stencil.circles);
		mirror_contours (stencil.contours);
	end mirror_stencil_objects;
	

	procedure rotate_stencil_objects (
		stencil	: in out type_stencil;
		angle	: in type_rotation_model)
	is begin
		rotate_lines (stencil.lines, angle);
		rotate_arcs (stencil.arcs, angle);
		rotate_circles (stencil.circles, angle);
		rotate_contours (stencil.contours, angle);
	end rotate_stencil_objects;


	procedure move_stencil_objects (
		stencil	: in out type_stencil;
		offset	: in type_distance_relative)
	is begin
		move_lines (stencil.lines, offset);
		move_arcs (stencil.arcs, offset);
		move_circles (stencil.circles, offset);
		move_contours (stencil.contours, offset);
	end move_stencil_objects;


	
	procedure arc_stencil_properties (
		face			: in type_face;
		cursor			: in pac_stencil_arcs.cursor;
		log_threshold 	: in type_log_level) 
	is
		use pac_stencil_arcs;
		arc : type_stencil_arc;
	begin
		arc := element (cursor);
		log (text => "solder paste (stencil) arc face" & to_string (face) & space 
			 & to_string (type_arc (arc))
			 & " width" & to_string (arc.width),
			 level => log_threshold);
	end arc_stencil_properties;

	
	procedure circle_stencil_properties (
		face			: in type_face;
		cursor			: in pac_stencil_circles.cursor;
		log_threshold 	: in type_log_level) 
	is
		use pac_stencil_circles;
	begin
		log (text => "solder paste (stencil) circle face" & to_string (face) & space 
			& to_string (element (cursor)),
			level => log_threshold);
	end;

	
	procedure line_stencil_properties (
		face			: in type_face;
		cursor			: in pac_stencil_lines.cursor;
		log_threshold 	: in type_log_level) 
	is
		use pac_stencil_lines;
		line : type_stencil_line;
	begin
		line := element (cursor);
		log (text => "solder paste (stencil) line face" & to_string (face) & space
			 & to_string (type_line (line))
			 & " width" & to_string (line.width),
			 level => log_threshold);
	end line_stencil_properties;

	
end et_stencil;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
