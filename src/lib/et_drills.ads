------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                              DRILLS                                      --
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

with et_pcb_coordinates;		use et_pcb_coordinates;
with et_logging;				use et_logging;


package et_drills is
	use pac_geometry_2;
	
	
	-- We fit the diameter in a reasonable range via a subtype:
	drill_size_min : constant type_distance_positive := 0.05;
	drill_size_max : constant type_distance_positive := 10.0;
	subtype type_drill_size is type_distance_positive range drill_size_min .. drill_size_max;
	
	-- Checks whether given drill size is in range of type_drill_size
	procedure validate_drill_size (drill : in type_distance);

	-- DRILLS
	type type_drill is tagged record
		position	: type_vector_model;
		diameter	: type_drill_size := drill_size_min;
		-- CS locked : type_locked;
	end record;

	-- returns the properties of the given drill as string:
	function to_string (drill : in type_drill) return string;


	subtype type_drill_size_tht is type_drill_size range 0.8 .. 5.0;
	
end et_drills;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
