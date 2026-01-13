------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                             FILE WRITE                                   --
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
-- To Do:
--
--

with ada.characters.latin_1;
with et_string_processing;		use et_string_processing;


package et_file_write is


	subtype type_tab_depth is natural range natural'first .. 9;
	tab_depth : type_tab_depth := type_tab_depth'first;
	
	tab : constant character := ada.characters.latin_1.ht;

	procedure tab_depth_up;
	procedure tab_depth_down;
	procedure reset_tab_depth;

	type type_section_mark is (HEADER, FOOTER);	

	
	procedure section_mark (section : in string; mark : in type_section_mark);

	

	-- Writes a line in the current output.
	procedure write (
		keyword 	: in string;
		parameters	: in string;
		wrap		: in boolean := false;  -- when true, parameters will be enclosed in qotes (like "BEL Systems")
		as_comment	: in boolean := false); -- when true, the whole line will be put as comment


end et_file_write;
