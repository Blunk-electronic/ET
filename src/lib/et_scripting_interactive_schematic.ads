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

with et_symbols;				use et_symbols;
with et_devices;				use et_devices;
with et_schematic;				use et_schematic;
with et_string_processing;		use et_string_processing;


package et_scripting_interactive_schematic is

	procedure unit_selection_cancelled (
		self : access gtk_menu_shell_record'class);

	-- If the operator selects a unit from the menu, 
	-- then this procedure is called:
	procedure unit_selected_on_invoke (
		self : access gtk_menu_item_record'class);

	procedure menu_propose_units_on_delete (
		device			: in type_name;
		units			: in pac_unit_names.list;
		log_threshold	: in type_log_level);
	
	-- Proposes units on a menu if list "units" has
	-- more than one item. 
	-- If "units" contains only one item, then
	-- this single unit will be granted to be drawn.
	-- If "units" is empty, nothing happens.
	procedure menu_propose_units_on_invoke (
		device			: in type_name; -- R2
		units			: in pac_unit_names.list;
		log_threshold	: in type_log_level);

	-- Makes the unit_move a selected unit.
	-- Append the cursors of the device and unit to the list
	-- proposed_units. Afterwards there will be only one single 
	-- item in that list.
	-- Sets the selected_unit. This signals the GUI which unit is to be
	-- drawn at the cursor or mouse position:
	procedure select_unit_for_move;

	-- The interactive completition process of 
	-- moving, dragging or rotating 
	-- a unit comes to an end here.
	procedure finish_unit_move;
	
	-- If the operator selects a unit from the menu, 
	-- then this procedure is called.
	-- The connected net segments are identified and collected in 
	-- container segments_being_dragged.
	procedure unit_selected_on_move (
		self : access gtk_menu_item_record'class);

	-- Proposes units on a menu if list "units" has
	-- more than one item. 
	-- If "units" contains only one item, then
	-- this single unit will be selected and 
	-- granted to be drawn.
	-- If we are about to drag a single unit, then the connected
	-- net segments are identified and collected in 
	-- container segments_being_dragged.	
	-- If "units" is empty, nothing happens.
	procedure menu_propose_units_on_move (
		units			: in pac_unit_names.list;
		log_threshold	: in type_log_level);


	-- Makes the placeholder_move a selected placeholder.
	-- Append the cursors of the device and unit to the list
	-- proposed_placeholders. Afterwards there will be only one single 
	-- item in that list.
	-- Sets the selected_placeholder. This signals the GUI which unit is to be
	-- drawn at the cursor or mouse position:
	procedure select_placeholder_for_move;

	-- The interactive completition process of 
	-- moving, dragging or rotating 
	-- a placeholder comes to an end here.
	procedure finish_placeholder_move;


-- SET VARIANT
	
	-- When setting the package variant, the cursor to
	-- the targeted device is stored here temporarily:
	set_variant_device : et_schematic.type_devices.cursor;
	
	-- Sets the package variant of the given device.
	-- The device must exist in the current active module.
	-- Otherwise constraint error rises.
	procedure set_variant (device : in et_devices.type_name);
	
	
end et_scripting_interactive_schematic;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
