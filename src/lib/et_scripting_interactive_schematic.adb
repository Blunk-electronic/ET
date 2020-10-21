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
with gtk.gentry;				use gtk.gentry;

with ada.text_io;				use ada.text_io;
with ada.containers;			use ada.containers;

with et_coordinates;			use et_coordinates;
with et_scripting;				use et_scripting;
with et_canvas_schematic;		use et_canvas_schematic;
use et_canvas_schematic.pac_canvas;

package body et_scripting_interactive_schematic is

	procedure unit_selection_cancelled (self : access gtk_menu_shell_record'class) is
	begin
		set_status ("Unit selection cancelled");
		--log (text => "Unit selection cancelled");
		--single_cmd_status.aborted := true;
	end unit_selection_cancelled;

	procedure unit_selected (self : access gtk_menu_item_record'class) is
		
		-- Extract the unit name from field 2 of the menu item:
		name : constant string := get_field_from_line (
			text_in		=> self.get_label,
			position	=> 2);
	begin
		--put_line ("Selected unit " & name & " via pull down menu.");
		set_status ("selected unit " & name);

		-- Append the unit name and the current sheet number to the command,
		-- append the number of the current active sheet,
		-- remove field 1 and 2 (domain and module name) and
		-- show the now extended command on the console:
		append_argument_to_command (
			cmd			=> single_cmd_status.cmd,
			argument	=> name,
			trim		=> false);
		
		append_argument_to_command (
			cmd			=> single_cmd_status.cmd,
			argument	=> to_sheet (current_active_sheet));

	end unit_selected;


	procedure menu_propose_units (									 
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
		
	begin -- menu_propose_units
		log (text => "proposing units ... ", level => log_threshold);

		if length (units) > 1 then
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

			log (text => "menu with" & count_type'image (length (units)) & " units is up",
				level => log_threshold + 1);
			
		else
			unit_name := element (units.first);

			--set_status ("auto-selected last available unit " & to_string (unit_name));
			--log (text => "auto-selected last available unit " & to_string (unit_name),
					--level => log_threshold + 1);

			set_status ("selected last available unit " & to_string (unit_name));
			
			-- Append the unit name to the command,
			-- append the number of the current active sheet,
			-- remove field 1 and 2 (domain and module name) and
			-- show the now extended command on the console:
			append_argument_to_command (
				cmd			=> single_cmd_status.cmd,
				argument	=> to_string (unit_name),
				trim		=> false);
			
			append_argument_to_command (
				cmd			=> single_cmd_status.cmd,
				argument	=> to_sheet (current_active_sheet));

		end if;		
		
	end menu_propose_units;

	
	
end et_scripting_interactive_schematic;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
