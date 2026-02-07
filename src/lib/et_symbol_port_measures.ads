------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                 SYMBOL PORT / MEASURES AND DIMENSIONS                    --
--                                                                          --
--                              S p e c                                     --
--                                                                          --
-- Copyright (C) 2017 - 2026                                                --
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

--   For correct displaying set tab width in your editor to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
--
-- DESCRIPTION:
--
-- A port is something where a net can be attached to.
-- Units of devices and netchangers use these kind of ports.
--
--
-- To Do:
-- -
--

with et_schematic_geometry;				use et_schematic_geometry;
with et_schematic_coordinates;			use et_schematic_coordinates;


package et_symbol_port_measures is

	use pac_geometry_2;


	subtype type_port_length is type_distance_positive
		range 2.0 .. 20.0;

	
	-- CS subtype of type_distance_positive
	
	
	-- A port is basically a line with a linewidth equal to those
	-- of net segments.
	-- Its start point is the port position.
	-- At the start point a net will be attached.
	-- The end point points towards the symbol body. Depending on the port
	-- rotation the end tail points:
	--  to the left if rotation is 0 degree. net attached from the right.
	--  to the right if rotation is 180 degree. net attached from the left.
	--  downwards if the rotation is 90 degree. net attached from above.
	--  upwards if the rotation is 270 degree. net attached from below.
	
	port_circle_line_width : constant type_distance_positive := 0.1;
	
	port_circle_radius : constant type_distance_positive := 0.8;

	-- The distance between port end point and port name:
	port_name_spacing : constant type_distance_positive := 2.0;


	
end et_symbol_port_measures;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
