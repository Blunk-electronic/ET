------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                         TERMINAL STOPMASK                                --
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


package et_terminal_stopmask is

	use pac_contours;

	

	type type_stopmask_expand_mode is (
		AS_PAD,			-- mask assumes same shape as conductor pad underneath
		EXPAND_PAD,		-- mask is sligtly greater thatn underlying conductor pad (definded by DRU)
		USER_SPECIFIC);	-- mask has user specific contours

	stopmask_expand_mode_default : constant 
		type_stopmask_expand_mode := EXPAND_PAD;


	function to_string (
		shape : in type_stopmask_expand_mode) 
		return string;

		
	function to_shape (
		shape : in string)
		return type_stopmask_expand_mode;






	type type_stopmask_contour is new type_contour with null record;
	-- CS other properties of stopmask contours ?

	-- Contours of stopmask are required only if the shape is user specific.
	-- Otherwise the shape is to be derived from the underlying conductor pad and
	-- the DRU settings:
	type type_stopmask_shape (
		expand_mode : type_stopmask_expand_mode := stopmask_expand_mode_default)
	is record
		case expand_mode is
			when USER_SPECIFIC => contour : type_stopmask_contour;
			when others => null;
		end case;
	end record;
	
	
end et_terminal_stopmask;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
