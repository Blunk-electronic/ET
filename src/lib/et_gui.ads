------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                             GUI GENERAL                                  --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 - 2022 Mario Blunk, Blunk electronic          --
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

with ada.text_io;				use ada.text_io;
with et_string_processing;		use et_string_processing;
with et_logging;				use et_logging;
with et_general;				use et_general;
with et_project;				use et_project;
with et_project.modules;		use et_project.modules;
with et_geometry;				use et_geometry;
with et_coordinates;

with gdk;						use gdk;
with gdk.event;					use gdk.event;

with gtk.main;
with gtk.window; 				use gtk.window;
with gtk.widget;  				use gtk.widget;
with gtk.box;					use gtk.box;
with gtk.button;     			use gtk.button;
with gtk.toolbar; 				use gtk.toolbar;
with gtk.tool_button;			use gtk.tool_button;
with gtk.enums;					use gtk.enums;
with gtk.gentry;				use gtk.gentry;
with gtk.frame;					use gtk.frame;
with gtk.scrolled_window;		use gtk.scrolled_window;
-- with gtk.combo_box_text;	with gtk.combo_box_text;	
-- with gtkada.style;			use gtkada.style;

with glib;						use glib;
with glib.object;				use glib.object;

package et_gui is

	-- Starts the GUI and displays the given module (schematic and board):
	procedure single_module (
		project			: in pac_project_name.bounded_string;	-- blood_sample_analyzer
		module			: in pac_generic_modules.cursor;				-- cursor of generic module
		sheet			: in et_coordinates.type_sheet := et_coordinates.type_sheet'first; -- the sheet to be opened
		script			: in pac_script_name.bounded_string; -- rename_nets.scr
		log_threshold	: in type_log_level
		);

	
end et_gui;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
