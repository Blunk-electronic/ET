------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                        CONDUCTOR TEXT PACKAGES                           --
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

with ada.text_io;			use ada.text_io;


package body et_conductor_text.packages is
	

	procedure iterate (
		texts	: in pac_conductor_texts.list;
		process	: not null access procedure (position : in pac_conductor_texts.cursor);
		proceed	: not null access boolean)
	is
		use pac_conductor_texts;
		c : pac_conductor_texts.cursor := texts.first;
	begin
		while c /= no_element and proceed.all = TRUE loop
			process (c);
			next (c);
		end loop;
	end iterate;
	

	procedure mirror_texts (
		texts	: in out pac_conductor_texts.list;
		axis	: in type_axis_2d := Y)
	is
		result : pac_conductor_texts.list;

		procedure query_text (c : in pac_conductor_texts.cursor) is 
			t : type_conductor_text := element (c);
		begin
			mirror_text (t);
			result.append (t);
		end query_text;
		
	begin
		texts.iterate (query_text'access);
		texts := result;
	end mirror_texts;


	procedure rotate_texts (
		texts	: in out pac_conductor_texts.list;
		angle	: in type_rotation)
	is
		result : pac_conductor_texts.list;

		procedure query_text (c : in pac_conductor_texts.cursor) is 
			t : type_conductor_text := element (c);
		begin
			rotate_text (t, angle);
			result.append (t);
		end query_text;

	begin
		texts.iterate (query_text'access);
		texts := result;
	end rotate_texts;



	procedure move_texts (
		texts	: in out pac_conductor_texts.list;
		offset	: in type_distance_relative)
	is
		result : pac_conductor_texts.list;

		procedure query_text (c : in pac_conductor_texts.cursor) is 
			t : type_conductor_text := element (c);
		begin
			move_text (t, offset);
			result.append (t);
		end query_text;

	begin
		texts.iterate (query_text'access);
		texts := result;
	end move_texts;

	
	function to_polygons (
		texts		: in pac_conductor_texts.list)
		return pac_polygon_list.list
	is
		result : pac_polygon_list.list;

		procedure query_text (c : in pac_conductor_texts.cursor) is
			text : type_conductor_text renames element (c);
			scratch : pac_polygon_list.list;
		begin
			scratch := get_borders (text.vectors);
			result.splice (before => pac_polygon_list.no_element, source => scratch);
		end query_text;
		
	begin
		texts.iterate (query_text'access);
		return result;
	end to_polygons;

	
		
end et_conductor_text.packages;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
