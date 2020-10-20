------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                        SCRIPTING EXCEPTIONS                              --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017- 2020 Mario Blunk, Blunk electronic           --
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

with gtk.menu;					use gtk.menu;
with gtk.menu_item;				use gtk.menu_item;
with gtk.menu_shell;			use gtk.menu_shell;

with et_devices;				use et_devices;
with et_string_processing;		use et_string_processing;


package et_scripting_interactive_schematic is

	procedure append_argument_to_command (
		cmd		: in out type_fields_of_line;
		argument: in string;
		trim	: in boolean := true);
	
	procedure unit_selection_cancelled (
		self : access gtk_menu_shell_record'class);

	procedure unit_selected (
		self : access gtk_menu_item_record'class);

	procedure menu_propose_units (									 
		units			: in pac_unit_names.list;
		log_threshold	: in type_log_level);

	
end et_scripting_interactive_schematic;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
