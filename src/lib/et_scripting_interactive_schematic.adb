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

with gtk.main;
with gtk.gentry;					use gtk.gentry;

with ada.text_io;					use ada.text_io;
with ada.containers;				use ada.containers;

with et_coordinates;				use et_coordinates;
with et_scripting;					use et_scripting;
with et_project.modules;			use et_project.modules;
with et_canvas_schematic;			use et_canvas_schematic;
use et_canvas_schematic.pac_canvas;

with et_canvas_schematic_units;		use et_canvas_schematic_units;

package body et_scripting_interactive_schematic is

	procedure unit_selection_cancelled (self : access gtk_menu_shell_record'class) is
	begin
		set_status ("Unit selection cancelled");
	end unit_selection_cancelled;

	procedure unit_selected (self : access gtk_menu_item_record'class) is
		
		-- Extract the unit name from field 2 of the menu item:
		name : constant string := get_field_from_line (
			text_in		=> self.get_label,
			position	=> 2);
	begin
		set_status ("selected unit " & name);

		unit_add.name := to_name (name);

		-- use the current primary tool for moving the unit:
		unit_add.tool := primary_tool;

		-- Allow drawing the unit:
		unit_add.via_invoke := true;

		single_cmd_status.finalization_pending := true;
		
		redraw;
	end unit_selected;


	procedure menu_propose_units_on_invoke (
		device			: in type_name;
		units			: in pac_unit_names.list;
		log_threshold	: in type_log_level)
	is
		use gtk.menu;
		use gtk.menu_item;
		use pac_unit_names;

		unit_name : type_unit_name.bounded_string;

		m : gtk_menu; -- the menu
		i : gtk_menu_item; -- an item on the menu

		procedure query_name (c : in pac_unit_names.cursor) is begin
			-- Build the menu item. NOTE: The actual unit name must be
			-- the 2nd string of the entry.
			i := gtk_menu_item_new_with_label (
				"unit " & to_string (element (c)));

			-- Connect the item with the "activate" signal:
			i.on_activate (unit_selected'access);

			m.append (i);
			i.show;
		end query_name;
		
	begin -- menu_propose_units_on_invoke
		log (text => "proposing units of " & to_string (device) 
			 & " for invoke ... ",
			 level => log_threshold);
		
		case length (units) is
			when 0 =>
				-- no menu required
				set_status ("No more units of " & to_string (device) & " available !");
				
			when 1 =>
				-- no menu required
				unit_name := element (units.first);

				set_status ("selected single available unit " 
					& to_string (unit_name)
					& " of " & to_string (device));
				
				unit_add.name := unit_name;

				-- use the current primary tool for moving the unit:
				unit_add.tool := primary_tool;
				
				-- Allow drawing the unit:
				unit_add.via_invoke := true;

				single_cmd_status.finalization_pending := true;
				
				redraw;

			when others =>
				-- show available units in a menu
				m := gtk_menu_new;

				-- In case the operator closes the menu (via ESC for example)
				m.on_cancel (unit_selection_cancelled'access);
				
				units.iterate (query_name'access);

				m.show;

				m.popup
					(
					-- CS func => set_position'access,
							
					-- button 0 means: this is not triggered by a key press
					-- or a button click:
					button => 0,
							
					-- get_current_event_time causes the menu to remain
					-- until a 2nd click.
					activate_time => gtk.main.get_current_event_time);

				set_status ("Please select unit via menu !");
			
		end case;
		
	end menu_propose_units_on_invoke;


	-- Called when the operator selects a unit from the menu.
	procedure unit_selected_on_move (self : access gtk_menu_item_record'class) is
		
		-- Extract the unit name from field 2 of the menu item:
		name : constant string := get_field_from_line (
			text_in		=> self.get_label,
			position	=> 2);
	begin
		set_status ("selected unit " & name);

		-- Now we know the unit name:
		unit_move.unit := to_name (name);

		-- Append the cursors of the device and unit to the list of proposed units.
		-- There will be only one single item in that list.
		proposed_units.append (new_item => (
			device	=> locate_device (current_active_module, unit_move.device),
			unit	=> locate_unit (current_active_module, unit_move.device, unit_move.unit)));

		-- Set the selected unit. This signals the GUI which unit is to be
		-- drawn at the cursor or mouse position:
		selected_unit := proposed_units.first;
		
		-- use the current primary tool for moving the unit:
		unit_move.tool := primary_tool;

		-- Allow drawing the unit:
		unit_move.being_moved := true;

		single_cmd_status.finalization_pending := true;
		
		redraw;
	end unit_selected_on_move;
	
	procedure menu_propose_units_on_move (
		units			: in pac_unit_names.list;
		log_threshold	: in type_log_level)
	is
		use gtk.menu;
		use gtk.menu_item;
		use pac_unit_names;
		use pac_proposed_units;
		
		m : gtk_menu; -- the menu
		i : gtk_menu_item; -- an item on the menu

		procedure query_name (c : in pac_unit_names.cursor) is begin
			-- Build the menu item. NOTE: The actual unit name must be
			-- the 2nd string of the entry.
			i := gtk_menu_item_new_with_label (
				"unit " & to_string (element (c)));

			-- Connect the item with the "activate" signal:
			i.on_activate (unit_selected_on_move'access);

			m.append (i);
			i.show;
		end query_name;
		
	begin -- menu_propose_units_on_move
		log (text => "proposing units of " & to_string (unit_move.device) 
			 & " for move ... ",
			 level => log_threshold);

		case length (units) is
			when 0 =>
				-- no menu required
				set_status ("No units of " & to_string (unit_move.device) & " on this sheet !");
				
			when 1 =>
				-- No menu required. We know the device and unit name:
				unit_move.unit := element (units.first);

				set_status ("selected single available unit " 
					& to_string (unit_move.unit)
					& " of " & to_string (unit_move.device));

				-- Append the cursors of the device and unit to the list of proposed units.
				-- There will be only one single item in that list.
				proposed_units.append (new_item => (
					device	=> locate_device (current_active_module, unit_move.device),
					unit	=> locate_unit (current_active_module, unit_move.device, unit_move.unit)));

				-- Set the selected unit. This signals the GUI which unit is to be
				-- drawn at the cursor or mouse position:
				selected_unit := proposed_units.first;
				
				-- use the current primary tool for moving the unit:
				unit_move.tool := primary_tool;
				
				-- Allow drawing the unit:
				unit_move.being_moved := true;

				single_cmd_status.finalization_pending := true;
				
				redraw;

			when others =>
				-- At the moment we know only the device name. 
				-- The unit name will be made known when the operator 
				-- selects a unit from the menu.
				
				-- show available units in a menu
				m := gtk_menu_new;

				-- In case the operator closes the menu (via ESC for example)
				m.on_cancel (unit_selection_cancelled'access);
				
				units.iterate (query_name'access);

				m.show;

				m.popup
					(
					-- CS func => set_position'access,
							
					-- button 0 means: this is not triggered by a key press
					-- or a button click:
					button => 0,
							
					-- get_current_event_time causes the menu to remain
					-- until a 2nd click.
					activate_time => gtk.main.get_current_event_time);

				set_status ("Please select unit via menu !");
			
		end case;
		
	end menu_propose_units_on_move;
	
	
end et_scripting_interactive_schematic;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
