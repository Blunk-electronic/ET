------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                        CONDUCTOR TEXT PACKAGES                           --
--                                                                          --
--                              S p e c                                     --
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



package et_conductor_text.packages is
	
	
	package pac_conductor_texts is new doubly_linked_lists (type_conductor_text);
	use pac_conductor_texts;

	
	-- Iterates the texts. Aborts the process when the proceed-flag goes false:
	procedure iterate (
		texts	: in pac_conductor_texts.list;
		process	: not null access procedure (position : in pac_conductor_texts.cursor);
		proceed	: not null access boolean);
	

	-- Mirrors a list of texts along the given axis:
	procedure mirror_texts (
		texts	: in out pac_conductor_texts.list;
		axis	: in type_axis_2d := Y);


	-- Rotates a list of texts by the given angle about the origin:
	procedure rotate_texts (
		texts	: in out pac_conductor_texts.list;
		angle	: in type_rotation_model);


	-- Moves a list of texts by the given offset:
	procedure move_texts (
		texts	: in out pac_conductor_texts.list;
		offset	: in type_distance_relative);


	-- Converts a list of texts to a list of polygons.
	-- Adresses the borders of the vector texts only.
	function to_polygons (
		texts		: in pac_conductor_texts.list)
		return pac_polygon_list.list;

	
end et_conductor_text.packages;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
