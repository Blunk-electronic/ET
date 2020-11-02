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
with et_modes.schematic;			use et_modes.schematic;
with et_schematic_ops;				use et_schematic_ops;
with et_canvas_schematic;			use et_canvas_schematic;
use et_canvas_schematic.pac_canvas;

with et_canvas_schematic_units;		use et_canvas_schematic_units;


package body et_scripting_interactive_schematic is

	procedure unit_selection_cancelled (self : access gtk_menu_shell_record'class) is
	begin
		set_status ("Unit selection cancelled");
	end unit_selection_cancelled;

	-- If unit names are proposed in a menu, then a single
	-- item on the menu is "unit C", "unit D", ...
	-- This function extracts the 2nd field from that entry
	-- and returns something like "C" or "D":
	function extract_unit_name (menu_item : in string) return string is begin
		return get_field_from_line (
			text_in		=> menu_item,
			position	=> 2);
	end extract_unit_name;


	
-- INVOKE

	procedure unit_selected_on_invoke (self : access gtk_menu_item_record'class) is
		name : constant string := extract_unit_name (self.get_label);
	begin
		set_status ("selected unit " & name);
		unit_add.name := to_name (name);

		-- use the current primary tool for moving the unit:
		unit_add.tool := primary_tool;

		-- Allow drawing the unit:
		unit_add.via_invoke := true;

		single_cmd_status.finalization_pending := true;
		
		redraw;
	end unit_selected_on_invoke;

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
			i.on_activate (unit_selected_on_invoke'access);

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


	

-- MOVE / DRAG / ROTATE
	
	procedure select_unit_for_move is begin
		-- Append the cursors of the device and unit to the list of proposed units.
		-- There will be only one single item in that list.
		proposed_units.append (new_item => (
			device	=> locate_device (current_active_module, unit_move.device),
			unit	=> locate_unit (current_active_module, unit_move.device, unit_move.unit)));

		-- Set the selected unit. This signals the GUI which unit is to be
		-- drawn at the cursor or mouse position:
		selected_unit := proposed_units.first;
	end select_unit_for_move;

	-- The interactive completition process of moving, dragging or rotating 
	-- a unit comes to an end here.
	procedure finish_unit_move is begin
		select_unit_for_move;
		
		-- use the current primary tool for moving the unit:
		unit_move.tool := primary_tool;

		case verb is
			when VERB_DRAG => 
				-- If we are about to drag a unit, then the connected
				-- net segments must be identified:
				find_attached_segments;

				-- Allow drawing the unit:
				unit_move.being_moved := true;

				single_cmd_status.finalization_pending := true;
				
			when VERB_MOVE => 
				-- Allow drawing the unit:
				unit_move.being_moved := true;

				single_cmd_status.finalization_pending := true;
				
			when VERB_ROTATE =>
				rotate_selected_unit;
				
			when others => null;
		end case;
		
		redraw;
	end finish_unit_move;

	procedure finish_placeholder_move is begin
		select_placeholder_for_move;

		-- use the current primary tool for moving the unit:
		placeholder_move.tool := primary_tool;

		-- Map from the curren noun to the category of the placeholder:
		case noun is
			when NOUN_NAME =>
				placeholder_move.category := NAME;

			when NOUN_PURPOSE =>
				placeholder_move.category := PURPOSE;

			when NOUN_VALUE =>
				placeholder_move.category := VALUE;

			when others => raise constraint_error; -- CS should never happen
		end case;
				
		-- Allow drawing the placeholder:
		placeholder_move.being_moved := true;

		single_cmd_status.finalization_pending := true;
		redraw;
	end finish_placeholder_move;

	
	procedure unit_selected_on_move (self : access gtk_menu_item_record'class) is
		name : constant string := extract_unit_name (self.get_label);
	begin
		set_status ("selected unit " & name);

		-- Now we know the unit name:
		case noun is
			when NOUN_UNIT =>
				unit_move.unit := to_name (name);

				finish_unit_move;
				
			when NOUN_NAME | NOUN_PURPOSE | NOUN_VALUE =>
				placeholder_move.unit := to_name (name);

				finish_placeholder_move;
				
			when others => raise constraint_error; -- CS should never happen
		end case;
				
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
		case noun is
			when NOUN_UNIT =>
				log (text => "proposing units of " & to_string (unit_move.device) & " ... ",
					level => log_threshold);

			when NOUN_NAME =>
				log (text => "proposing units of " & to_string (placeholder_move.device) & " ... ",
					 level => log_threshold);

			when others => null;
		end case;

				
		case length (units) is
			when 0 => -- no menu required
				case noun is
					when NOUN_UNIT =>
						set_status ("No units of " & to_string (unit_move.device) & " on this sheet !");

					when NOUN_NAME | NOUN_PURPOSE | NOUN_VALUE =>
						set_status ("No units of " & to_string (placeholder_move.device) & " on this sheet !");

					when others => raise constraint_error; -- CS should never happen
				end case;
				
			when 1 => -- No menu required. We know the device and unit name:
				case noun is
					when NOUN_UNIT =>
						unit_move.unit := element (units.first);

						set_status ("selected single available unit " 
							& to_string (unit_move.unit)
							& " of " & to_string (unit_move.device));
						
						finish_unit_move;

						
					when NOUN_NAME | NOUN_PURPOSE | NOUN_VALUE =>
						placeholder_move.unit := element (units.first);

						set_status ("selected single available unit " 
							& to_string (placeholder_move.unit)
							& " of " & to_string (placeholder_move.device));
						
						finish_placeholder_move;
						
					when others => raise constraint_error; -- CS should never happen
				end case;
				

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
	


	procedure select_placeholder_for_move is begin
		-- Append the cursors of the device and unit to the list of proposed placeholders.
		-- There will be only one single item in that list.
		proposed_placeholders.append (new_item => (
			device	=> locate_device (current_active_module, placeholder_move.device),
			unit	=> locate_unit (current_active_module, placeholder_move.device, placeholder_move.unit)));

		-- Set the selected placeholder. This signals the GUI which placeholder is to be
		-- drawn at the cursor or mouse position:
		selected_placeholder := proposed_placeholders.first;
	end select_placeholder_for_move;

	
end et_scripting_interactive_schematic;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
