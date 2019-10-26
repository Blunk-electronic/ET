------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                               FRAMES                                     --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2019 Mario Blunk, Blunk electronic                 --
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

with et_general;				use et_general;
with et_geometry;

package et_frames is

-- PAPER SIZES
    type type_paper_size is (A3, A4); -- CS: others ?
    paper_size_default : constant type_paper_size := A4;

	function to_paper_size (paper_size : in string) return type_paper_size;
	function to_string (paper_size : in type_paper_size) return string;

	type type_paper_orientation is (PORTRAIT, LANDSCAPE);
	
	
	generic
		with package shapes is new et_geometry.shapes_2d (<>);
		
	package frames is
		use shapes.geometry;
		use shapes;

		-- PAPER SIZES
		-- As default we assume landscape format for all sheets.
		paper_size_A3_x : constant type_distance_positive := 420.0;
		paper_size_A3_y : constant type_distance_positive := 297.0;
		
		paper_size_A4_x : constant type_distance_positive := 297.0;
		paper_size_A4_y : constant type_distance_positive := 210.0;

		function paper_dimension (
		-- Returns for the given paper size, orientation and axis the correspoinding size in mm.
			paper_size	: in type_paper_size;
			orientation	: in type_paper_orientation := LANDSCAPE;
			axis		: in type_axis_2d)
			return type_distance_positive;


	
	end frames;
	
	
end et_frames;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
