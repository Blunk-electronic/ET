------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                     SYMBOL PORT / GENERAL PROPERTIES                     --
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

--   For correct displaying set tab width in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--
-- DESCRIPTION:
--
-- This package is about general properties of ports that
-- are used by schematic symbols and netchanger symbols.
--
--
--   history of changes:
--
--
--
-- To Do:
--
--


with et_symbol_port_measures;			use et_symbol_port_measures;
with et_schematic_geometry;				use et_schematic_geometry;



package et_symbol_port_general is

	use pac_geometry_2;

	
	port_length_default : constant type_port_length := 2.5;



	-- A port is basically a line with a linewidth equal to those
	-- of net segments (global constant net_linewidth).
	-- Its start point is the port position.
	-- At the start point a net will be attached.
	-- The end point points towards the symbol body. Depending on the port
	-- rotation the end tail points:
	--  to the left if rotation is 0 degree. net attached from the right.
	--  to the right if rotation is 180 degree. net attached from the left.
	--  downwards if the rotation is 90 degree. net attached from above.
	--  upwards if the rotation is 270 degree. net attached from below.
	
	type type_port_general is tagged record
		-- This is the place where a net is connected:
		position	: type_vector_model;

		-- From the position a line starts.
		-- This line represents a port.
		-- The linewidth is the global constant net_linewidth:
		length		: type_port_length := port_length_default; 
		
		--rotation	: et_schematic_coordinates.type_rotation_model := 0.0; -- CS use type_rotation_model_relative ?
		rotation	: type_rotation_relative := 0.0;
		--  90.0 -- to be connected with a net from above,
		-- -90.0 -- from below,
		-- 180.0 -- from the left,
		--   0.0 -- from the right
	end record;

	

	-- CS primitive operations
	-- move, rotate, mirror
	-- get_position, get_rotation

	
end et_symbol_port_general;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
