------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                             NET JUNCTION                                 --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2025                                                --
-- Mario Blunk / Blunk electronic                                           --
-- Buchfinkenweg 3 / 99097 Erfurt / Germany                                 --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
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


with et_schematic_coordinates;	use et_schematic_coordinates;
with et_logging;				use et_logging;


package et_net_junction is

	use pac_geometry_2;

	
	procedure junction_in_sloping_segment (
		point : in type_object_position);
	
	
	-- A net junction is where segments are connected with each other.
	type type_junctions is record
		A	: boolean := false; -- CS dedicated type like type_junction_active 
								-- and value like JUNCTION_ON, JUNCTION_OFF
		B	: boolean := false;
	end record;

	
	-- GUI relevant only: In the schematic editor, the junction is drawn as follows:
	junction_radius : constant type_distance_positive := 0.5;
	

	
	junction_symbol : type_circle := type_circle (to_circle (
			center	=> origin,
			radius 	=> junction_radius));




	
end et_net_junction;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
