------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                   SCHEMATIC OPERATIONS ON SHEETS                         --
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
--   ToDo: 

with et_module;							use et_module;
with et_generic_modules;				use et_generic_modules;
with et_drawing_frame;					use et_drawing_frame;
with et_drawing_frame.schematic;		use et_drawing_frame.schematic;
with et_schematic_text;					use et_schematic_text;
with et_sheets;							use et_sheets;
with et_logging;						use et_logging;
with et_cmd_origin_to_commit;			use et_cmd_origin_to_commit;


package et_schematic_ops_sheets is

	use pac_generic_modules;


	procedure sheet_not_found (
		sheet : in type_sheet);
	

	-- Returns the total number of sheets
	-- that the module currently has:
	function get_sheet_count (
		module	: in pac_generic_modules.cursor)
		return type_sheet;
		
		
		
	-- Returns true if the given sheet exists:
	function sheet_exists (
		module	: in pac_generic_modules.cursor;
		sheet	: in type_sheet)
		return boolean;
	

							  
	-- Returns the description of a sheet of a generic module:
	function get_sheet_description (
		module	: in pac_generic_modules.cursor;
		sheet	: in type_sheet)
		return type_schematic_description;
	


	-- Sets the category of the given sheet.
	-- Assumes that the sheet exists. Otherwise an exception
	-- is raised:
	procedure set_sheet_category (
		module_cursor	: in pac_generic_modules.cursor;
		sheet			: in type_sheet;
		category		: in type_schematic_sheet_category;
		commit_design	: in type_commit_design := DO_COMMIT;
		log_threshold	: in type_log_level);
	

	
	-- Deletes the given sheet.
	-- Assumes that the sheet exists. Otherwise an exception
	-- is raised:
	procedure delete_sheet (
		module_cursor	: in pac_generic_modules.cursor;
		sheet			: in type_sheet;
		commit_design	: in type_commit_design := DO_COMMIT;
		log_threshold	: in type_log_level);
	
	
	
	-- CS subprograms to get and set the title block position ?
	-- currently this is defined in the frame template file *.frs
	

end et_schematic_ops_sheets;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
