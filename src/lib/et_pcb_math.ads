------------------------------------------------------------------------------
--                                                                          --
--                          SYSTEM ET PCB MATHEMATICS                       --
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
--   ToDo: 

with et_pcb_coordinates;		use et_pcb_coordinates;


package et_pcb_math is

	
	function distance (point_one, point_two : in type_point_3d) return type_distance_total;
	-- Computes the total distance between point_one and point_two.
		
	function arc_end_point (
	-- Computes the end point of an arc.
		center		: in type_point_3d;
		start_point	: in type_point_3d;	
		angle 		: in type_angle)
		return type_point_3d;
		
	
end et_pcb_math;

-- Soli Deo Gloria
