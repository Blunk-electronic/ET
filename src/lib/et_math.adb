------------------------------------------------------------------------------
--                                                                          --
--                          SYSTEM ET MATHEMATICS                           --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               B o d y                                    --
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

-- with ada.text_io;				use ada.text_io;
-- with ada.strings.maps;			use ada.strings.maps;
-- with ada.strings.bounded;       use ada.strings.bounded;
-- with ada.strings.unbounded; 	use ada.strings.unbounded;
-- with ada.containers;            use ada.containers;
-- with ada.containers.vectors;
-- with ada.containers.doubly_linked_lists;
-- with ada.containers.indefinite_doubly_linked_lists;
-- with ada.containers.ordered_maps;
-- with ada.containers.indefinite_ordered_maps;
-- with ada.containers.ordered_sets;
-- 
with et_coordinates;			use et_coordinates;

with et_string_processing;


package body et_math is

	procedure dummy is begin null; end dummy;
	
-- 	function round (
-- 	-- Rounds the given float_in according to the given accuracy.
-- 	-- Accuracy must be a power of ten (0.01, 0.1, 1, 10, ..).
-- 		float_in : in et_coordinates.type_distance;
-- 		accuracy : in et_coordinates.type_distance) return et_coordinates.type_distance is
-- 		use et_coordinates;
-- 		use et_string_processing;

-- 		package functions_distance is new ada.numerics.generic_elementary_functions (type_distance);
-- 		use functions_distance;
-- 		
-- 		a : type_distance;
-- 
-- 		d : type_distance;
-- 		i : integer;
-- 	begin
		-- CS: check if accuracy is in type_accuracy
-- 		if type_distance (accuracy) in type_accuracy then
-- 			null;
-- 		end if;

-- 		a := accuracy ** (-1.0); -- the reciprocal of the accuracy
-- 		d := float_in * a; -- multiply the given float_in by the reciprocal of the accuracy
-- 		i := integer (d); -- round result to integer
-- 		d := type_distance (i); -- convert result back to float and divide by reciprocal of accuracy
-- 		d := d / a;
		
-- 		return d;
-- 		return float_in;
-- 	end round;


	
	
end et_math;

-- Soli Deo Gloria
