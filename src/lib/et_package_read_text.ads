------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                        PACKAGE READ / TEXT                               --
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
--
-- DESCRIPTION:
-- 
-- This is about lines, arcs and circles in the stopmask.
--
--   do do:
--
--

with et_string_processing;				use et_string_processing;

with et_board_text;						use et_board_text;
with et_device_placeholders.packages;	use et_device_placeholders.packages;
with et_conductor_text.packages;		use et_conductor_text.packages;
with et_package_model;					use et_package_model;
-- with et_pcb_sides;						use et_pcb_sides;
with et_logging;						use et_logging;


package et_package_read_text is

	use pac_text_board_vectorized;
	use pac_texts_fab_with_content;
	



	--pac_text				: et_packages.type_text_with_content;
	--pac_text				: pac_text_fab.type_text_fab;
	pac_text				: type_text_fab_with_content; -- CS rename
	--content					: et_text.pac_text_content.bounded_string;
	pac_text_placeholder	: type_text_placeholder; -- CS rename




	procedure read_text (
		line : in type_fields_of_line);
	
	
	procedure read_placeholder (
		line : in type_fields_of_line);

	
	
end et_package_read_text;
