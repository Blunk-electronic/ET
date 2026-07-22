------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                             GUI GENERAL                                  --
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
--                                                                          --
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

with et_logging;				use et_logging;
with et_project_name;			use et_project_name;
with et_generic_modules;		use et_generic_modules;
with et_sheets;
with et_script_names;			use et_script_names;


-- with gtk.combo_box_text;	with gtk.combo_box_text;
-- with gtkada.style;			use gtkada.style;


package et_gui_2 is

	use pac_generic_modules;

	-- Starts the GUI and displays the given module (schematic and board):
	procedure single_module (
		project			: in pac_project_name.bounded_string;	-- blood_sample_analyzer
		module			: in pac_generic_modules.cursor;				-- cursor of generic module
		sheet			: in et_sheets.type_sheet := et_sheets.type_sheet'first; -- the sheet to be opened
		script			: in pac_script_name.bounded_string; -- rename_nets.scr
		log_threshold	: in type_log_level
		);

	
end et_gui_2;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
