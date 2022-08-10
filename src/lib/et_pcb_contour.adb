------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                           PCB CONTOURS                                   --
--                                                                          --
--                              B o d y                                     --
--                                                                          --
--         Copyright (C) 2017 - 2022 Mario Blunk, Blunk electronic          --
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
--   to do:

with ada.text_io;				use ada.text_io;


package body et_pcb_contour is

	--procedure iterate (
		--holes	: in pac_holes_as_polygons.list;
		--process	: not null access procedure (position : in pac_holes_as_polygons.cursor);
		--proceed	: not null access boolean)
	--is
		--c : pac_holes_as_polygons.cursor := holes.first;
	--begin
		--while c /= pac_holes_as_polygons.no_element and proceed.all = TRUE loop
			--process (c);
			--next (c);
		--end loop;
	--end iterate;

	
	
	function to_polygons (
		holes		: in pac_holes.list;
		tolerance	: in type_distance_positive)
		return pac_holes_as_polygons.list
	is
		use et_contour_to_polygon;
		
		result : pac_holes_as_polygons.list;

		-- Iterate the given list of holes and convert each hole
		-- to a polygon. The polygon in turn will be appended to the result
		-- so that a list of polygons will be the result.
		
		procedure query_hole (c : in pac_holes.cursor) is begin

			result.append (to_polygon (
				contour		=> element (c),
				tolerance	=> tolerance));
			
		end query_hole;

		
	begin
		holes.iterate (query_hole'access);
		return result;
	end to_polygons;
	

	procedure offset_holes (
		holes		: in out pac_holes_as_polygons.list;
		offset		: in type_distance_positive;
		debug		: in boolean := false)
	is
		result : pac_holes_as_polygons.list;
	
		procedure query_hole (c : in pac_holes_as_polygons.cursor) is
			p : type_polygon := element (c);
		begin
			if debug then
				put_line (" hole in : " & to_string (element (c)));
			end if;
			
			offset_polygon (p, type_float_internal_positive (offset));

			if debug then
				put_line (" hole out: " & to_string (p));
			end if;

			result.append (p);
		end query_hole;
			
	begin
		if debug then
			put_line ("offsetting holes by " & to_string (offset) & "mm");
		end if;
		
		holes.iterate (query_hole'access);

		holes := result;
		
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
