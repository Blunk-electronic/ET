------------------------------------------------------------------------------
--                                                                          --
--                    SYSTEM ET GEOMETRY                                    --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 Mario Blunk, Blunk electronic                 --
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

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with et_coordinates;

package et_geometry is

	type type_distance_point_from_line is record
		distance		: et_coordinates.type_distance;
		out_of_range	: boolean;
	end record;

	type type_line_range is ( inside_end_points, with_end_points, beyond_end_points);
	
	function distance_of_point_from_line (
	-- Computes the shortest distance (perpendicular) of a given point from the given line.
	-- If the point is outside the
	-- range of the x coordinate, the corresponding flag in the return value is set.
		point, line_start, line_end: in et_coordinates.type_2d_point;
		line_range : in type_line_range) return type_distance_point_from_line;

-- 	procedure move (
-- 		point : in out type_2d_point;
-- 		offset : in type_2d_point
-- 		);

-- 	function move (
-- 		point	: in type_2d_point'class;
-- 		offset	: in type_2d_point
-- 		) return type_2d_point'class;
-- 
-- 	function mirror (
-- 		point	: in type_2d_point;
-- 		axis	: in type_axis
-- 		) return type_2d_point;

	
end et_geometry;

-- Soli Deo Gloria
