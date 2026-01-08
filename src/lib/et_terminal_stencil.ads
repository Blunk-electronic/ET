------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                         TERMINAL STENCIL                                 --
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
--   history of changes:
--
-- to do:
--


with et_board_geometry;			use et_board_geometry;


package et_terminal_stencil is

	use pac_geometry_2;
	use pac_contours;



	-- The contour of the stencil is at first taken from the pad geometry
	-- and then modified:
	type type_stencil_shrink_mode is (
		AS_PAD,			-- opening in stencil has the same size as the conductor pad underneath
		SHRINK_PAD,		-- opening sligtly smaller than conductor pad. defined by shrink_factor
		USER_SPECIFIC);	-- opening has a user defined outline

	--subtype type_stencil_shrink is type_polygon_scale range 0.2 .. 1.0;
	--stencil_shrink_default : constant type_stencil_shrink := 0.7; -- CS adjust to a useful value

	stencil_shrink_mode_default : constant type_distance_positive := 0.7; -- CS adjust to a useful value
	-- CS subtype for shrink value ?
	
	stencil_modification_default : constant type_stencil_shrink_mode := AS_PAD;

	
	function to_string (
		shape : in type_stencil_shrink_mode) 
		return string;
		
		
	function to_modification (
		shape : in string) 
		return type_stencil_shrink_mode;
		
		
		

	type type_stencil_contours is new type_contour with null record;
	-- CS other properties stencil contours ?
	
	type type_stencil_shape (
		shrink_mode : type_stencil_shrink_mode := stencil_modification_default) 
	is record
		case shrink_mode is
			when USER_SPECIFIC =>
				contour : type_stencil_contours;
				
			when SHRINK_PAD =>
				shrink_factor : type_distance_positive := stencil_shrink_mode_default;
				
			when others => null;
		end case;
	end record;
	

	
	
	
end et_terminal_stencil;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
