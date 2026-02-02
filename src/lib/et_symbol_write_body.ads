------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                          SYMBOL WRITE / BODY                             --
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

--   For correct displaying set tab with in your edtior to 4.

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
--
-- DESCRIPTION:
-- This package is about primitive objects (lines, arcs, circles)
-- the symbol body is made of.


with et_logging;						use et_logging;
with et_symbol_model;					use et_symbol_model;


package et_symbol_write_body is


	procedure write_body_lines (
		symbol			: in type_symbol_model;
		log_threshold	: in type_log_level);
		
	
	procedure write_body_arcs (
		symbol			: in type_symbol_model;
		log_threshold	: in type_log_level);


	procedure write_body_circles (
		symbol			: in type_symbol_model;
		log_threshold	: in type_log_level);

	
	
	
end et_symbol_write_body;
