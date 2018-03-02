------------------------------------------------------------------------------
--                                                                          --
--                          SYSTEM ET GEOMETRY                              --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2018 Mario Blunk, Blunk electronic                 --
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

--   For correct displaying set tab with in your edtior to 4.

--   The two letters "CS" indicate a "construction side" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with et_coordinates;

package et_geometry is

	type type_distance_point_from_line is record -- CS: should be private
		distance		: et_coordinates.type_distance := et_coordinates.zero_distance;
		sits_on_start	: boolean := false;
		sits_on_end		: boolean := false;
		out_of_range	: boolean := false;
	end record;

	type type_line_range is (
		inside_end_points,	-- start and end point excluded
		with_end_points,	-- start and end point included
		beyond_end_points	-- unlimited line assumed. extends beyond both start and end point into infinity
		);
	
	function distance_of_point_from_line (
	-- Computes the shortest distance (perpendicular) of a given point from the given line.
	-- If the point is outside the
	-- range of the x coordinate, the corresponding flag in the return value is set.
		point, line_start, line_end: in et_coordinates.type_2d_point;
		line_range : in type_line_range) return type_distance_point_from_line;

end et_geometry;

-- Soli Deo Gloria
