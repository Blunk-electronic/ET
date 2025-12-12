------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                           PCB CONTOUR                                    --
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

with ada.text_io;				use ada.text_io;


package body et_pcb_contour is



	procedure iterate (
		holes	: in pac_holes.list;
		process	: not null access procedure (position : in pac_holes.cursor);
		proceed	: not null access boolean)
	is
		c : pac_holes.cursor := holes.first;
	begin
		while c /= pac_holes.no_element and proceed.all = TRUE loop
			process (c);
			next (c);
		end loop;
	end iterate;
	


	

	procedure mirror_holes (
		holes	: in out pac_holes.list;
		axis	: in type_mirror := MIRROR_ALONG_Y_AXIS)
	is
		result : pac_holes.list;

		procedure query_hole (c : in pac_holes.cursor) is
			hole : type_hole := element (c);
		begin
			mirror (hole);
			result.append (hole);
		end query_hole;
		
	begin
		holes.iterate (query_hole'access);
		holes := result;
	end mirror_holes;


	procedure rotate_holes (
		holes	: in out pac_holes.list;
		angle	: in type_rotation_model)
	is
		result : pac_holes.list;

		procedure query_hole (c : in pac_holes.cursor) is
			hole : type_hole := element (c);
		begin
			rotate_by (hole, angle);
			result.append (hole);
		end query_hole;
		
	begin
		holes.iterate (query_hole'access);
		holes := result;
	end rotate_holes;



	procedure move_holes (
		holes	: in out pac_holes.list;
		offset	: in type_vector_model)
	is
		result : pac_holes.list;

		procedure query_hole (c : in pac_holes.cursor) is
			hole : type_hole := element (c);
		begin
			move_by (hole, offset);
			result.append (hole);
		end query_hole;
		
	begin
		--put_line ("move holes");
		holes.iterate (query_hole'access);
		holes := result;
	end move_holes;


	
	
	function to_polygons (
		holes		: in pac_holes.list;
		tolerance	: in type_distance_positive)
		return pac_polygon_list.list
	is
		use et_contour_to_polygon;
		
		result : pac_polygon_list.list;

		-- Iterate the given list of holes and convert each hole
		-- to a polygon. The polygon in turn will be appended to the result
		-- so that a list of polygons will be the result.
		
		procedure query_hole (c : in pac_holes.cursor) is begin

			result.append (to_polygon (
				contour		=> element (c),
				mode		=> EXPAND,						  
				tolerance	=> tolerance));
			
		end query_hole;

		
	begin
		holes.iterate (query_hole'access);
		return result;
	end to_polygons;
	

	
	procedure offset_holes (
		holes			: in out pac_polygon_list.list;
		offset			: in type_distance_positive;
		log_threshold	: in type_log_level)
	is
		use pac_polygon_offsetting;
		
		--use pac_polygon_list;
		--result : pac_polygon_list.list;
	
		--procedure query_hole (c : in pac_polygon_list.cursor) is
			--p : type_polygon := element (c);
		--begin
			--if debug then
				--put_line (" hole in : " & to_string (element (c)));
			--end if;
			
			--offset_polygon (p, type_float_positive (offset));

			--if debug then
				--put_line (" hole out: " & to_string (p));
			--end if;

			--result.append (p);
		--end query_hole;
			
	begin
		log (text => "offsetting holes by " & to_string (offset) & "mm",
			 level => log_threshold);


		offset_polygons (holes, type_float_positive (offset), log_threshold + 1);
		
		--holes.iterate (query_hole'access);

		--holes := result;
		
		--if debug then
			--put_line (count_type'image (holes.length));
		--end if;

		--if debug then
			--put_line (to_string (holes.first_element));
		--end if;
		
	end offset_holes;


	
end et_pcb_contour;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
