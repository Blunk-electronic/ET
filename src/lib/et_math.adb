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

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
--   ToDo: 
--		1. Objects like net segments, net labels, junctions, notes ... 
--		   should be collected in ordered sets instead of doubly_linked_lists
--			- the benefits: placing identical objects at the same position would be impossible
--			- the cons: ordering subprograms required

with ada.text_io;				use ada.text_io;
with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded;       use ada.strings.bounded;
with ada.strings.unbounded; 	use ada.strings.unbounded;
with ada.containers;            use ada.containers;
with ada.containers.vectors;
with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_doubly_linked_lists;
with ada.containers.ordered_maps;
with ada.containers.indefinite_ordered_maps;
with ada.containers.ordered_sets;

with et_coordinates;			use et_coordinates;

with et_string_processing;


package body et_math is

	function round (
		float_in : in et_coordinates.type_distance;
		accuracy : in et_coordinates.type_distance) return et_coordinates.type_distance is
		use et_coordinates;
		use et_string_processing;

		package functions_distance is new ada.numerics.generic_elementary_functions (type_distance);
		use functions_distance;
		
-- 		type type_accuracy is delta 0.001 digits 10;
		a : type_distance;

		d : type_distance;
		i : integer;
	begin
		-- CS: check if accuracy is in type_accuracy
-- 		if type_distance (accuracy) in type_accuracy then
-- 			null;
-- 		end if;
		a := accuracy ** (-1.0);
		d := float_in * a;
		i := integer (d);
		d := type_distance (i);
		d := d / a;
		
		return d;
	end round;


	
	
end et_math;

-- Soli Deo Gloria
