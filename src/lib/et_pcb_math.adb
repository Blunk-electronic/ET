------------------------------------------------------------------------------
--                                                                          --
--                          SYSTEM ET PCB MATHEMATICS                       --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               B o d y                                    --
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

--   For correct displaying set tab width in your edtior to 4.

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

with et_general;				use et_general;
with et_pcb_coordinates;		use et_pcb_coordinates;
with ada.numerics.generic_elementary_functions;

with et_string_processing;


package body et_pcb_math is

	function arc_end_point (
	-- Computes the end point of an arc.
		center		: in type_point_2d;
		start_point	: in type_point_2d;	
		angle 		: in type_rotation)
		return type_point_2d is
		use geometry;
		end_point : type_point_2d; -- to be returned
	begin
		-- CS
		set (X, zero, end_point);
		set (Y, zero, end_point);
		
		return end_point;
	end arc_end_point;
	
end et_pcb_math;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
