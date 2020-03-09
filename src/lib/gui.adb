------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                             GUI GENERAL                                  --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2020 Mario Blunk, Blunk electronic          --
--                                                                          --
--    This program is free software: you can redistribute it and/or modify  --
--    it under the terms of the GNU General Public License as published by  --
--    the Free Software Foundation, either version 3 of the License, or     --
--    (at your option) any later version.                                   --
--                                                                          --
--    This program is distributed in the hope that it will be useful,       --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of        --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         --
--    GNU General Public License for more details.                          --
--                                                                          --
--    You should have received a copy of the GNU General Public License     --
--    along with this program.  If not, see <http://www.gnu.org/licenses/>. --
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

with gtk.main;

with ada.text_io;				use ada.text_io;

with et_general;				use et_general;
with et_project;				use et_project;
with et_string_processing;		use et_string_processing;

with gui_schematic;
with gui_board;

package body gui is

	procedure single_module (
		module			: in type_modules.cursor; -- cursor of generic module to be edited
		sheet			: in et_coordinates.type_sheet := et_coordinates.type_sheet'first; -- the sheet to be opened
		log_threshold	: in type_log_level) is
	begin
		log (text => "launching mode " & to_string (MODE_MODULE), level => log_threshold);
		log (text => "opening module " & enclose_in_quotes (to_string (type_modules.key (module))), level => log_threshold);
		log (text => "sheet" & to_sheet (sheet), level => log_threshold);

		gtk.main.init; -- initialize the main gtk stuff

		-- set up the schematic window
		gui_schematic.init_window (module, sheet, log_threshold + 1);

		-- CS test if board available (see et_schematic.type_module)
		
		-- set up the board window
		gui_board.init_window (module, log_threshold + 1);



		
		-- Start the main gtk loop. This is a loop that permanently draws the widgets and
		-- samples them for possible signals sent.
		gtk.main.main;
		
	end single_module;
	
	
end gui;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
