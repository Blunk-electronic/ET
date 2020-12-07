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

with gdk;
with gdk.event;						use gdk.event;
with gdk.types;						use gdk.types;
with gdk.types.keysyms;				use gdk.types.keysyms;

with gtk.main;
with gtk.widget;					use gtk.widget;
with gtk.gentry;					use gtk.gentry;

with ada.text_io;					use ada.text_io;
with ada.containers;				use ada.containers;

with et_geometry;
with et_coordinates;				use et_coordinates;
with et_packages;
with et_schematic;					use et_schematic;
with et_scripting;					use et_scripting;
with et_modes.schematic;			use et_modes.schematic;
with et_schematic_ops;				use et_schematic_ops;
with et_schematic_ops.units;
with et_canvas_schematic;			use et_canvas_schematic;
use et_canvas_schematic.pac_canvas;

with et_canvas_schematic_units;		use et_canvas_schematic_units;


package body et_scripting_interactive_schematic is

	procedure status_select_unit is begin
		set_status ("Please select unit via menu !");
	end status_select_unit;
	
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


-- DELETE

	procedure unit_selected_on_delete (self : access gtk_menu_item_record'class) is
		name : constant string := extract_unit_name (self.get_label);
	begin
		unit_delete.unit := to_name (name);

		et_schematic_ops.units.delete_unit (
			module_cursor	=> current_active_module,
			device_name		=> unit_delete.device,
			unit_name		=> unit_delete.unit,
			log_threshold	=> log_threshold + 1);

		status_clear;
		
		redraw;
	end unit_selected_on_delete;
	
	procedure menu_propose_units_on_delete (
		device			: in type_device_name;
		units			: in pac_unit_names.list;
		log_threshold	: in type_log_level)
	is
		use gtk.menu;
		use gtk.menu_item;
		use pac_unit_names;

		unit_name : pac_unit_name.bounded_string;

		m : gtk_menu; -- the menu
		i : gtk_menu_item; -- an item on the menu

		procedure query_name (c : in pac_unit_names.cursor) is begin
			-- Build the menu item. NOTE: The actual unit name must be
			-- the 2nd string of the entry.
			i := gtk_menu_item_new_with_label (
				"unit " & to_string (element (c)));

			-- Connect the item with the "activate" signal:
			i.on_activate (unit_selected_on_delete'access);

			m.append (i);
			i.show;
		end query_name;
		
	begin -- menu_propose_units_on_delete
		log (text => "proposing units of " & to_string (device) 
			 & " for deleting ... ",
			 level => log_threshold);
		
		case length (units) is
			when 0 =>
				-- no menu required
				set_status ("No more units of " & to_string (device) & " available !");
				
			when 1 =>
				-- no menu required
				unit_name := element (units.first);
				
				unit_delete.unit := unit_name;

				et_schematic_ops.units.delete_unit (
					module_cursor	=> current_active_module,
					device_name		=> unit_delete.device,
					unit_name		=> unit_delete.unit,
					log_threshold	=> log_threshold + 1);

				status_clear;
				
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

				status_select_unit;
		end case;
		
	end menu_propose_units_on_delete;
	
	
	
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
		device			: in type_device_name;
		units			: in pac_unit_names.list;
		log_threshold	: in type_log_level)
	is
		use gtk.menu;
		use gtk.menu_item;
		use pac_unit_names;

		unit_name : pac_unit_name.bounded_string;

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
			 & " for invoking ... ",
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

				status_select_unit;
			
		end case;
		
	end menu_propose_units_on_invoke;


	

-- MOVE / DRAG / ROTATE
	
	procedure select_unit_for_move is
		use pac_proposed_units;
		su : type_selected_unit;

		pos : pac_geometry_sch.type_point;
		use et_geometry;
	begin
		-- Append the cursors of the device and unit to the list of proposed units.
		-- There will be only one single item in that list.
		proposed_units.append (new_item => (
			device	=> locate_device (current_active_module, unit_move.device),
			unit	=> locate_unit (current_active_module, unit_move.device, unit_move.unit)));

		-- Set the selected unit. This signals the GUI which unit is to be
		-- drawn at the cursor or mouse position:
		selected_unit := proposed_units.first;


		-- Move the cursor to the unit:
		su := element (selected_unit);

		-- Get the x/y position of the unit:
		pos := pac_geometry_sch.type_point (position (
				device	=> su.device,
				unit	=> su.unit));

		canvas.move_cursor (ABSOLUTE, cursor_main, pos);
		
	end select_unit_for_move;

	-- Maps from the current noun to the category of the placeholder.
	-- May be called when the noun is NOUN_NAME, NOUN_PURPOSE or NOUN_VALUE.
	-- If the noun is something different, then a constraint_error is raised.
	function to_category return type_placeholder_meaning is begin
		case noun is
			when NOUN_NAME		=> return NAME;
			when NOUN_PURPOSE	=> return PURPOSE;
			when NOUN_VALUE		=> return VALUE;
			when others			=> raise constraint_error; -- CS should never happen
		end case;
	end to_category;							 
	
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
				
			when others => raise constraint_error; -- CS should never happen
		end case;
		
		redraw;
	end finish_unit_move;

	procedure finish_placeholder_move is begin
		select_placeholder_for_move;

		-- use the current primary tool for moving the unit:
		placeholder_move.tool := primary_tool;

		-- Map from the current noun to the category of the placeholder:
		placeholder_move.category := to_category;

		case verb is
			when VERB_MOVE =>
				
				-- Allow drawing the placeholder:
				placeholder_move.being_moved := true;

				single_cmd_status.finalization_pending := true;

			when VERB_ROTATE =>
				rotate_selected_placeholder (placeholder_move.category);
				reset_placeholder;

			when others => raise constraint_error; -- CS should never happen
		end case;
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

		-- The number of given units determines the
		-- next actions:
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

				status_select_unit;
			
		end case;
		
	end menu_propose_units_on_move;
	


	procedure select_placeholder_for_move is
		use pac_proposed_placeholders;
		sp : type_selected_placeholder;
		
		pos : pac_geometry_sch.type_point;
		use et_geometry;

	begin
		-- Append the cursors of the device and unit to the list of proposed placeholders.
		-- There will be only one single item in that list.
		proposed_placeholders.append (new_item => (
			device	=> locate_device (current_active_module, placeholder_move.device),
			unit	=> locate_unit (current_active_module, placeholder_move.device, placeholder_move.unit)));

		-- Set the selected placeholder. This signals the GUI which placeholder is to be
		-- drawn at the cursor or mouse position:
		selected_placeholder := proposed_placeholders.first;



		
		-- Move the cursor to the placeholder:
		sp := element (selected_placeholder);
			
		-- Get the x/y position of the placeholder:
		pos := position (
				device		=> sp.device,
				unit		=> sp.unit,
				category	=> to_category); -- maps from noun to placeholder category

		canvas.move_cursor (ABSOLUTE, cursor_main, pos);
	
	end select_placeholder_for_move;



	-- Once the operator selects a package variant from the menu, then
	-- this procedure is called. The variant will be applied immediately
	-- after selection:
	procedure variant_selected (self : access gtk.menu_item.gtk_menu_item_record'class) is
	begin
		-- Apply the variant to the device. The device is indicated by the
		-- global variable set_variant_device. It has been set by 
		-- the other procedure set_variant (see below):
		set_variant (
			module	=> current_active_module,
			device	=> set_variant_device,
			variant	=> extract_variant_name (self.get_label));

		redraw;
	end variant_selected;

	--function variant_selection_key_event (
		--self	: access gtk_widget_record'class;
		--event	: gdk.event.gdk_event_key) 
		--return boolean 
	--is
		--key : gdk_key_type := event.keyval;

		---- This is required in order to propagate the key-pressed event further.
		--result : boolean; -- to be returned. Indicates that the event has been handled.
	--begin
		--case key is
			--when GDK_ESCAPE =>
				----put_line ("key A");

				---- Close the device selection if operator hits ESC:
				--package_variants_menu.destroy;
				--result := true;

			--when others =>
				----put_line ("key B");
				--result := false;
		--end case;
		
		--return result;		
	--end variant_selection_key_event;
	
	procedure set_variant (device : in et_devices.type_device_name) is
		use pac_variants;
		variants : pac_variants.map;
		device_cursor_sch : et_schematic.type_devices.cursor;

		procedure show_variants_menu is
			m : gtk_menu;
			i : gtk_menu_item;
			
			procedure query_variant (c : in pac_variants.cursor) is begin
				-- Build the menu item:
				i := gtk_menu_item_new_with_label (to_package_variant_item (c));
				i.on_activate (variant_selected'access);
				m.append (i);
				i.show;
			end query_variant;
			
		begin
			m := gtk_menu_new;
			--m.on_key_press_event (variant_selection_key_event'access);
			
			variants.iterate (query_variant'access);

			m.show;
			m.popup;

		end show_variants_menu;
		
	begin -- set_variant
		device_cursor_sch := locate_device (current_active_module, device);

		-- Setting a package variant is possible for real devices only:
		if is_real (device_cursor_sch) then		
			variants := get_available_variants (current_active_module, device);

			if length (variants) > 1 then

				-- Store device cursor temporarily here.
				-- Required by procedure variant_selected. See above.
				set_variant_device := device_cursor_sch;
				
				show_variants_menu;
			else
				set_status ("Device has only one package variant !");
			end if;
		else
			set_status ("ERROR : Device is virtual and does not have a package !");
		end if;

	end set_variant;


	procedure set_property (device : in et_devices.type_device_name) is
		su : type_selected_unit;
	begin
		-- If the properties window is already open, then the window
		-- is moved to the foreground.
		if not window_properties_is_open then
		
			-- Mark the whoe device as selected:
			
			su.device := locate_device (current_active_module, device);
			-- su.unit does not matter here because we care about the device name only.
			-- It is left as it is: no_element.
			-- The drawing operation for units regards the whole device as
			-- selected, thus highlighting all units.

			-- We use the container for proposed units althrough there will be
			-- only one element it it:
			proposed_units.append (su);
			selected_unit := proposed_units.first;

			-- Open the properties window:
			window_set_property;
		else
			-- Move the properties window to the foreground so that the operator
			-- is notified about the already open properties window:
			window_properties.window.present;
		end if;
	end set_property;
	
end et_scripting_interactive_schematic;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
