------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                     NETCHANGER SYMBOL IN BOARD                           --
--                                                                          --
--                               S p e c                                    --
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
-- ToDo: 
--
--


with et_board_geometry;			use et_board_geometry;
with et_fonts;					use et_fonts;


package et_netchanger_symbol_board is

	use pac_geometry_2;


	name_to_origin_offset : constant type_distance_positive := 1.5;

	layer_id_to_origin_offset : constant type_distance_positive := 1.0;
	
	
	-- The size of the name (like N31):
	name_size : constant type_distance_positive := 1.0;

	-- The size of the layer id:
	layer_size : constant type_distance_positive := 1.0;


	
	netchanger_name_font : constant type_font :=
		to_font (FAMILY_MONOSPACE, SLANT_NORMAL, WEIGHT_NORMAL);


	
	linewidth_box : constant type_distance_positive := 0.25;

	line_point : constant type_distance_positive := 0.7;

	-- The symbol looks like an X:
	type type_netchanger_symbol is record
		line_1 : type_line := type_line (to_line (
			A => (-line_point, -line_point), 
			B => ( line_point,  line_point)));

		line_2 : type_line := type_line (to_line (
			A => (-line_point,  line_point), 
			B => ( line_point, -line_point)));
	end record;


	netchanger_symbol_board : constant 
		type_netchanger_symbol := (others => <>);
	
	
end et_netchanger_symbol_board;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
