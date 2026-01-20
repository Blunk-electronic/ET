------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         CANVAS BOARD DEVICES                             --
--                                                                          --
--                               B o d y                                    --
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
-- DESCRIPTION:
-- 

with ada.text_io;					use ada.text_io;

with glib;
with glib.values;

with gtk.file_chooser;
with gtk.file_filter;
with gtk.main;
with gtk.label;
with gtk.cell_renderer_text;
with gtk.list_store;
with gtk.tree_model;

with et_meta;

with et_conventions;
with et_generic_modules;			use et_generic_modules;
with et_canvas_board;				use et_canvas_board;
with et_schematic_ops_device;		use et_schematic_ops_device;
with et_schematic_ops.groups;
with et_board_ops;
with et_board_ops.devices;			use et_board_ops.devices;
with et_board_ops.groups;

with et_device_prefix;
with et_device_property_level;
with et_devices_electrical;				use et_devices_electrical;
with et_devices_electrical.units;
with et_devices_electrical.packages;	use et_devices_electrical.packages;
with et_devices_non_electrical;			use et_devices_non_electrical;

with et_package_library;
with et_package_model_name;
with et_package_read;

with et_modes.board;
with et_undo_redo;
with et_commit;
with et_directory_and_file_ops;
with et_object_status;				use et_object_status;

-- with et_canvas_board_preliminary_object;	use et_canvas_board_preliminary_object;


package body et_canvas_board_devices is

	use et_canvas_board.pac_canvas;
	use et_canvas_board.pac_device_ops;
	use pac_generic_modules;
	use pac_devices_electrical;
	use pac_devices_non_electrical;
	
	

	

	-- Outputs the name of a device in the status bar:
	procedure show_selected_device ( -- CS no need anymore ?
		name			: in type_device_name;
		electrical		: in boolean;
		clarification	: in boolean := false)
	is 
		praeamble_electric     : constant string := "selected device: ";
		praeamble_non_electric : constant string := "selected non-electrical device: ";
	begin
		if electrical then
			if clarification then
				set_status (praeamble_electric & to_string (name)
					& ". " & status_next_object_clarification);
			else
				set_status (praeamble_electric & to_string (name));
			end if;
		
		else
			if clarification then
				set_status (praeamble_non_electric & to_string (name)
					& ". " & status_next_object_clarification);
			else
				set_status (praeamble_non_electric & to_string (name));
			end if;
		end if;
	end show_selected_device;



	
	procedure show_selected_object (
		object		: in type_object)
	is 
		praeamble_electric     : constant string := "selected device: ";
		praeamble_non_electric : constant string := "selected non-electrical device: ";
	begin
		case object.cat is
			when CAT_ELECTRICAL_DEVICE =>
				set_status (praeamble_electric & get_device_name (object.electrical_device.cursor)
					& ". " & status_next_object_clarification);

			when CAT_NON_ELECTRICAL_DEVICE =>
				set_status (praeamble_non_electric & get_device_name (object.non_electrical_device.cursor)
					& ". " & status_next_object_clarification);

			when CAT_PLACEHOLDER =>
				set_status ("selected placeholder: " & to_string (object.placeholder));
				-- CS
				
			when CAT_VOID => null; -- CS
		end case;
	end show_selected_object;

	
	
	



	
	procedure clarify_object is 

		procedure do_it is
			use pac_objects;
			
			-- Gather all proposed objects:
			proposed_objects : constant pac_objects.list := 
				get_objects (active_module, PROPOSED, log_threshold + 1);

			proposed_object : pac_objects.cursor;

			-- We start with the first object that is currently selected:
			selected_object : type_object := 
				get_first_object (active_module, SELECTED, log_threshold + 1);

		begin
			log (text => "proposed objects total " 
				& natural'image (get_count (proposed_objects)),
				level => log_threshold + 2);

			
			-- Locate the selected object among the proposed objects:
			proposed_object := proposed_objects.find (selected_object);

			-- Deselect the the proposed object:
			modify_status (
				module_cursor	=> active_module, 
				operation		=> to_operation (CLEAR, SELECTED),
				object_cursor	=> proposed_object, 
				log_threshold	=> log_threshold + 1);

			-- Advance to the next proposed object:
			next (proposed_object);

			-- If end of list reached, then proceed at 
			-- the begin of the list:
			if proposed_object = pac_objects.no_element then
				proposed_object := proposed_objects.first;
			end if;
			
			-- Select the proposed object:
			modify_status (
				module_cursor	=> active_module, 
				operation		=> to_operation (SET, SELECTED),
				object_cursor	=> proposed_object, 
				log_threshold	=> log_threshold + 1);

			-- Display the object in the status bar:
			show_selected_object (element (proposed_object));
		end do_it;
		
		
	begin
		log (text => "clarify_object", level => log_threshold + 1);

		log_indentation_up;
		
		do_it;
		
		log_indentation_down;
	end clarify_object;



	

	-- This procedure searches for the first selected object
	-- and sets its status to "moving":
	procedure set_first_selected_object_moving is
		
		procedure do_it is
			-- Get the first selected object:
			selected_object : constant type_object := 
				get_first_object (active_module, SELECTED, log_threshold + 1);

			-- Gather all selected objects:
			objects : constant pac_objects.list :=
				get_objects (active_module, SELECTED, log_threshold + 1);

			use pac_objects;
			c : pac_objects.cursor;
		begin
			-- Get a cursor to the candidate object
			-- among all selected objects:
			c := objects.find (selected_object);

			if has_element (c) then
				log (text => "modify status", level => log_threshold + 1);
			
				modify_status (active_module, c, to_operation (SET, MOVING), log_threshold + 1);
			else
				log (ERROR, text => "nothing found");
			end if;
		end do_it;
		
		
	begin
		log (text => "set_first_selected_object_moving ...", level => log_threshold);
		log_indentation_up;
		do_it;
		log_indentation_down;
	end set_first_selected_object_moving;


	

	

	procedure find_objects (
		point : in type_vector_model)
	is 
		use et_modes.board;
		
		-- The total number of objects that have
		-- been proposed:
		count_total : natural := 0;


		-- This procedure searches for the first proposed
		-- object and marks it as "selected":
		procedure select_first_proposed is
			object : type_object := get_first_object (
						active_module, PROPOSED, log_threshold + 1);
		begin
			modify_status (
				active_module, object, to_operation (SET, SELECTED), log_threshold + 1);

			-- If only one object found, then show it in the status bar:
			if count_total = 1 then
				show_selected_object (object);
			end if;
		end select_first_proposed;


		-- This procedure proposes objects according to
		-- the current verb and noun:		 
		procedure propose is begin
			-- Before locating any objects, previous
			-- proposed or selected objects should be reset
			-- in both schematic and board editor:
			et_schematic_ops.groups.reset_objects (
				active_module, log_threshold + 1);

			et_board_ops.groups.reset_objects (
				active_module, log_threshold + 1);

			
			-- Propose objects according to current verb and noun:
			case verb is
				when VERB_MOVE | VERB_FLIP | VERB_ROTATE | VERB_SHOW =>
					
					case noun is
						when NOUN_DEVICE =>
			
							-- Propose electrical devices in the vicinity of the given point:
							propose_electrical_devices (
								module_cursor	=> active_module,
								catch_zone		=> set_catch_zone (point, get_catch_zone (catch_zone_radius_default)),
								count			=> count_total,
								log_threshold	=> log_threshold + 1);

							-- Propose non-electrical devices in the vicinity of the given point:
							propose_non_electrical_devices (
								module_cursor	=> active_module,
								catch_zone		=> set_catch_zone (point, get_catch_zone (catch_zone_radius_default)),
								count			=> count_total,
								log_threshold	=> log_threshold + 1);

							
						when NOUN_PLACEHOLDER =>

							-- Propose placeholders in the vicinity of the given point:
							propose_placeholders (
								module_cursor	=> active_module,
								catch_zone		=> set_catch_zone (point, get_catch_zone (catch_zone_radius_default)),
								count			=> count_total,
								log_threshold	=> log_threshold + 1);

							
						when others =>
							null; -- CS
					end case;


				when VERB_COPY | VERB_DELETE | VERB_RENAME =>
					
					case noun is
						when NOUN_DEVICE =>

							-- Propose non-electrical devices in the vicinity of the given point:
							propose_non_electrical_devices (
								module_cursor	=> active_module,
								catch_zone		=> set_catch_zone (point, get_catch_zone (catch_zone_radius_default)),
								count			=> count_total,
								log_threshold	=> log_threshold + 1);

						when others =>
							null; -- CS
					end case;

					
				when others =>
					null; -- CS
			end case;
		end propose;

		
	begin
		log (text => "find_objects", level => log_threshold);
		log_indentation_up;

		propose;

		log (text => "proposed objects total" & natural'image (count_total),
			 level => log_threshold + 1);

		
		-- Evaluate the number of objects found here:
		case count_total is
			when 0 =>
				null; -- nothing to do
				
			when 1 =>
				set_edit_process_running;
				select_first_proposed;

				if verb = VERB_MOVE then
					set_first_selected_object_moving;
				end if;
				
				reset_request_clarification;
				
			when others =>
				--log (text => "many objects", level => log_threshold + 2);
				set_request_clarification;
				select_first_proposed;
		end case;
		
		log_indentation_down;
	end find_objects;


	







	procedure reset_device_add is begin
		device_add := (others => <>);
	end;



	procedure rotate_device_add is begin
		if device_add.valid then
			add (device_add.rotation, 90.0);
		end if;
	end rotate_device_add;


	

	


	function get_top_most_important_library return string is
		use et_meta;
		use et_meta.pac_preferred_libraries_board;
		all_lib_dirs : pac_preferred_libraries_board.list;
		top_lib_dir : pac_preferred_library_board.bounded_string;

		use et_directory_and_file_ops;
		use et_board_ops;
	begin
		all_lib_dirs := get_preferred_libraries (active_module);
		top_lib_dir := element (all_lib_dirs.first);
		
		return expand (to_string (top_lib_dir));
	end get_top_most_important_library;


	


	
	procedure cb_model_directory_selected (
		button : access gtk_file_chooser_button_record'class) 
	is begin
		log (text => "cb_model_directory_selected", level => log_threshold);
		
		log_indentation_up;
		log (text => "directory: " & button.get_current_folder,
			 level => log_threshold + 1);
		
		log_indentation_down;
	end cb_model_directory_selected;



	


	procedure cb_package_model_selected (
		button : access gtk_file_chooser_button_record'class) 
	is
		use et_package_model_name;
		
		-- The selected package model file (*.pac) is stored here:
		package_model_file : pac_package_model_file.bounded_string;
		
		-- This cursor points to the package model in the library:
		use pac_package_models;
		package_cursor_lib : pac_package_models.cursor;
		

	begin -- cb_package_model_selected
		log (text => "cb_package_model_selected", level => log_threshold);

		-- Once the operator has started selecing a package model, the
		-- counter that counts the number of ESC hits until a reset 
		-- is perfomed, must be reset:
		reset_escape_counter;
		
		log_indentation_up;

		-- Get the name of the device model file from the button:
		package_model_file := to_package_model_name (button.get_filename);
		
		log (text => "selected package model file: " & to_string (package_model_file), 
			 level => log_threshold + 1);
		
		log_indentation_up;
		
		-- Read the package model file and store it in the 
		-- rig wide package library.
		-- If the package is already in the library, then nothing happpens:
		et_package_read.read_package (
			file_name		=> package_model_file, -- ../lbr/packages/SOT23.pac
			log_threshold	=> log_threshold + 2);
		-- CS add error flag output by read_package and evaluate accordingly.
		-- Wrap follwing actions in a procedure.
		-- CS use package cursor output by read_package instead 
		-- the following statement.
		
		-- Locate the package in the library:
		package_cursor_lib := get_package_model (package_model_file);

		-- Assign the cursor to the device_add:
		device_add.packge := package_cursor_lib;

		-- This is about a new device being added to the module.
		-- No value has been assigned yet. For this reason we
		-- assign the default value as defined in the device model.
		-- CS unit_add.value := get_default_value (package_cursor_lib);
				
		log_indentation_down;
		log_indentation_down;

		status_clear;
	end cb_package_model_selected;


	



	procedure cb_package_prefix_selected (
		combo : access gtk_combo_box_record'class) 
	is
		-- Get the model and active iter from the combo box:
		use gtk.tree_model;
		model : constant gtk_tree_model := combo.get_model;
		iter : constant gtk_tree_iter := combo.get_active_iter;

		use glib.values;
		name : gvalue;

		use et_device_prefix;
		prefix : pac_device_prefix.bounded_string; -- FD1
	begin
		log (text => "cb_package_prefix_selected", level => log_threshold);
		log_indentation_up;
		
		-- Get the prefix of the entry column 0:
		gtk.tree_model.get_value (model, iter, 0, name);

		prefix := to_prefix (glib.values.get_string (name));
		
		log (text => "selected prefix: " & to_string (prefix),
			 level => log_threshold + 1);
		
		-- Assign the prospective device name:
		device_add.device_pre := get_next_available_device_name (
			active_module, prefix, log_threshold + 1);
		
		-- Once the operator has started selecting a package variant, the
		-- counter that counts the number of ESC hits until a reset 
		-- is perfomed, must be reset:
		reset_escape_counter;

		-- The initial rotation is always zero:
		device_add.rotation := 0.0;
		
		-- Now the information in device_add is complete.
		-- By setting the flag "valid" the draw operation of the package
		-- starts drawing the package as it is sticking at the current tool
		-- (cursor or mouse):
		device_add.valid := true;

		focus_canvas;
		
		log_indentation_down;
	end cb_package_prefix_selected;



	
	
	

	procedure show_package_model_selection is
		use gtk.box;
		use gtk.label;
		use gtk.file_chooser;
		

		-- The button for the directory:		
		button_model_directory : gtk_file_chooser_button;

		-- The button for the model file:
		button_model_file : gtk_file_chooser_button;
		
		
		-- This procedure creates a button by which the operator
		-- selects the directory where a package model can be taken from:
		procedure make_button_directory is 
			box_directory : gtk_vbox;
			label_directory : gtk_label;
		begin
			-- Make a box and insert it in the properties box:
			gtk_new_vbox (box_directory, homogeneous => false);
			pack_start (box_v4, box_directory, padding => box_properties_spacing);

			-- Makre a label for the box and insert it in the box_directory:
			gtk_new (label_directory, "Model Directory");
			pack_start (box_directory, label_directory, padding => box_properties_spacing);

			-- Create a button by which the operator can select a directory:
			gtk_new (
				button		=> button_model_directory,
				title		=> "Select a Directory",
				action		=> ACTION_SELECT_FOLDER);

			-- CS: Currently the button_directory shows only the most important
			-- library path. It would be convenient if the operator would be shown
			-- all preferred library paths sorted by their rank.
			if button_model_directory.set_current_folder_uri (get_top_most_important_library) then
				null; -- Testing the existence of the folder is not required.
			end if;

			-- Insert the button_model_directory in the box_directory:
			pack_start (box_directory, button_model_directory, padding => box_properties_spacing);

			-- Connect the "on_file_set" signal with procedure
			-- cb_model_directory_selected:
			button_model_directory.on_file_set (cb_model_directory_selected'access);
			
			-- NOTE: Key pressed events are handled by the main window.
		end make_button_directory;

		

		-- This procedure creates a button by which the operator
		-- selects the model file (*.dev):
		procedure make_button_model is 
			box_model : gtk_vbox;
			label_model : gtk_label;
			
			use et_directory_and_file_ops;
			use et_package_model_name;
			use gtk.file_filter;
			file_filter : gtk_file_filter;
		begin
			-- Make a box and insert it in the properties box:
			gtk_new_vbox (box_model, homogeneous => false);
			pack_start (box_v4, box_model, padding => box_properties_spacing);

			-- Make a label for the box and insert it in the box_model:
			gtk_new (label_model, "Model File");
			pack_start (box_model, label_model, padding => box_properties_spacing);

			-- Create a file filter so that only *.dev files are shown
			-- to the operator:
			gtk_new (file_filter);
			add_pattern (file_filter, make_filter_pattern (package_model_file_extension));
			set_name (file_filter, "Package Models");

			-- Create a button by which the operator can select a model file:
			gtk_new (
				button		=> button_model_file,
				title		=> "Select a Package Model",
				action		=> ACTION_OPEN);

			-- Add the file filter to the button_model_file:
			button_model_file.add_filter (file_filter);

			if button_model_file.set_current_folder (button_model_directory.get_current_folder_uri) then
				null; -- Testing the existence of the package model is not required.
			end if;

			-- Insert the button_model_file in the box_model:
			pack_start (box_model, button_model_file, padding => box_properties_spacing);

			-- Connect the "on_file_set" signal with procedure cb_package_model_selected:
			button_model_file.on_file_set (cb_package_model_selected'access);

			-- NOTE: Key pressed events are handled by the main window.
		end make_button_model;



		-- This procedure builds the box_prefix with a
		-- combo box inside that allows
		-- the operator to select a package variant for the
		-- currently selected device model:
		procedure make_combo_box_prefix is
			use gtk.label;			
			use gtk.cell_renderer_text;
			use gtk.list_store;

			box_prefix : gtk_vbox;
			label_prefix : gtk_label;
			store : gtk_list_store;			
			render	: gtk_cell_renderer_text;

			-- This is the combo box that allows the operator
			-- to select among a list of prefixes.
			-- It is filled with allowed prefixes each time the operator selects
			-- a new package model file:
			cbox_prefix : gtk_combo_box;
			
		begin
			-- put_line ("make_combo_box_prefix");

			-- Create the box_prefix and insert it in the properties box:
			gtk_new_vbox (box_prefix, homogeneous => false);
			pack_start (box_v4, box_prefix, padding => box_properties_spacing);

			-- Create a label for the box and insert it in box_prefix:
			gtk_new (label_prefix, "prefix");
			pack_start (box_prefix, label_prefix, padding => box_properties_spacing);

			-- Create the storage model for the content of the combo box:
			make_store_for_prefixes (et_conventions.device_prefixes, store);
			
			-- Create the combo box:
			gtk_new_with_model (
				combo_box	=> cbox_prefix,
				model		=> +store); -- ?

			-- Insert the combo box in box_variant:
			pack_start (box_prefix, cbox_prefix, padding => box_properties_spacing);

			-- Connect the "on_changed" signal with procedure
			-- cb_package_prefix_selected:
			cbox_prefix.on_changed (cb_package_prefix_selected'access);

			-- The purpose of this stuff is unclear, but it
			-- is required to make the entries in the combo box visible:
			gtk_new (render);
			pack_start (cbox_prefix, render, expand => true);
			add_attribute (cbox_prefix, render, "markup", 0); -- column 0

			-- Show the box_variant with all its content:
			box_prefix.show_all;			
		end make_combo_box_prefix;


		
		
	begin
		log (text => "show_package_model_selection", level => log_threshold);
		log_indentation_up;
		
		-- Before inserting any widgets, the properties box must be cleared:
		clear_out_properties_box;

		-- Since the device model selection (re)starts here,
		-- a possible still active preview must be turned off:
		device_add.valid := false;

		-- Build the elements of the properties bar:
		make_button_directory;
		make_button_model;
		make_combo_box_prefix;

		-- Show the properties box:
		box_v4.show_all;

		log_indentation_down;		
	end show_package_model_selection;

	
	





	
	procedure add_non_electrical_device (
		place : in type_vector_model)
	is 
		use et_pcb_sides;
		use et_package_library;
		use pac_package_models;
		
		use et_modes.board;
		use et_commit;
		use et_undo_redo;

		-- Build the full package position from the
		-- given place, with rotation as given by device_add and on the
		-- top side of the board:
		position : constant type_package_position := 
			to_package_position (place, device_add.rotation, TOP);
	begin
		log (text => "add_non_electrical_device", level => log_threshold);
		log_indentation_up;
		
		-- Commit the current state of the design:
		commit (PRE, verb, noun, log_threshold);
		
		add_non_electrical_device (
			module_cursor	=> active_module,
			package_model	=> get_package_model_file (device_add.packge),
			position		=> position,
			prefix			=> get_prefix (device_add.device_pre),
			log_threshold	=> log_threshold + 1);

		-- Commit the new state of the design:
		commit (POST, verb, noun, log_threshold);

		status_clear;

		redraw_board;

		-- In case further devices are to be added,
		-- assign the prospective next device name:
		device_add.device_pre := get_next_available_device_name (
			active_module, get_prefix (device_add.device_pre), log_threshold + 1);
		
		log_indentation_down;
	end add_non_electrical_device;




	


-- COPY:	
	

	procedure copy_object (
		tool	: in type_tool;
		point	: in type_vector_model)
	is 
		
		-- Deletes the selected object:
		procedure finalize is
			use et_modes.board;
			use et_undo_redo;
			use et_commit;

			object : type_object := get_first_object (
					active_module, SELECTED, log_threshold + 1);
		begin
			log (text => "finalize copy", level => log_threshold);
			log_indentation_up;

			-- If a selected object has been found, then
			-- we do the actual finalizing:
			if object.cat /= CAT_VOID then

				reset_status_objects (active_module, log_threshold + 1);
				
				-- Commit the current state of the design:
				commit (PRE, verb, noun, log_threshold + 1);

				-- Do the copy operation:
				copy_object (
					module_cursor	=> active_module, 
					object			=> object, 
					destination		=> point,
					log_threshold	=> log_threshold + 1);

				-- Commit the new state of the design:
				commit (POST, verb, noun, log_threshold + 1);

				-- If a device has been copied, then the board
				-- must be redrawn:
				if object.cat = CAT_NON_ELECTRICAL_DEVICE then
					redraw_board;
				end if;
				
			else
				log (text => "nothing to do", level => log_threshold);
			end if;
				
			log_indentation_down;			

			-- clear status bar
			status_clear;

			-- The preview-object is no longer required:
			reset_device_add;
			
			reset_editing_process; -- prepare for a new editing process
		end finalize;


		
		-- After the original object has been selected, and the the operator is moving 
		-- the pointer or the cursor, a preview of the copied object is attached to
		-- the tool. The "preview object" is floating:
		procedure build_preview is

			-- Get the selected original object:
			object : type_object := get_first_object (
					active_module, SELECTED, log_threshold + 1);

		begin
			case object.cat is
				when CAT_NON_ELECTRICAL_DEVICE =>
					-- Build the preview of the device that is going to
					-- be added:
					device_add.packge := get_package_model (object.non_electrical_device.cursor);

					device_add.value := get_value (object.non_electrical_device.cursor);
					
					device_add.device_pre := get_next_available_device_name (
						active_module, get_prefix (object.non_electrical_device.cursor), log_threshold + 1);

					device_add.rotation := get_rotation (object.non_electrical_device.cursor);

					device_add.valid := true;
					
				when others =>
					-- CS
					null;
			end case;
		end build_preview;

		
		
	begin
		-- Initially the editing process is not running:
		if not edit_process_running then
		
			-- Set the tool being used:
			object_tool := tool;
			
			if not clarification_pending then
				-- Locate all objects in the vicinity of the given point:
				find_objects (point);
				-- NOTE: If many objects have been found, then
				-- clarification is now pending.

				-- If find_objects has found only one object,				
				-- then the flag edit_process_running is set true.

				-- Build the floating "preview-object" that is attached
				-- to the tool being used:
				if edit_process_running then
					build_preview;
				end if;
				
			else
				-- Here the clarification procedure ends.
				-- An object has been selected via procedure clarify_object.

				-- Build the floating "preview-object" that is attached
				-- to the tool being used:
				build_preview;

				set_edit_process_running;
				reset_request_clarification;
			end if;

		else
			finalize;
		end if;			
	end copy_object;





	
	
	
	
-- MOVE:


	procedure move_object (
		tool	: in type_tool;
		point	: in type_vector_model)
	is 
		
		-- Assigns the final position after the move to the selected object.
		-- Resets variable preliminary_object:
		procedure finalize is
			use et_modes.board;
			use et_undo_redo;
			use et_commit;

			object : constant type_object := get_first_object (
					active_module, SELECTED, log_threshold + 1);
		begin
			log (text => "finalize move", level => log_threshold);
			log_indentation_up;

			-- If a selected object has been found, then
			-- we do the actual finalizing:
			if object.cat /= CAT_VOID then

				reset_status_objects (active_module, log_threshold + 1);
				
				-- Commit the current state of the design:
				commit (PRE, verb, noun, log_threshold + 1);
				
				move_object (
					module_cursor	=> active_module, 
					object			=> object, 
					destination		=> snap_to_grid (point),
					log_threshold	=> log_threshold + 1);

				-- Commit the new state of the design:
				commit (POST, verb, noun, log_threshold + 1);

			else
				log (text => "nothing to do", level => log_threshold);
			end if;
				
			log_indentation_down;			
			
			status_clear;

			reset_editing_process; -- prepare for a new editing process
		end finalize;


		
	begin
		-- Initially the editing process is not running:
		if not edit_process_running then

			-- Set the tool being used:
			object_tool := tool;
			
			if not clarification_pending then
				-- Locate all devices in the vicinity of the given point:
				find_objects (point);
				-- NOTE: If many devices have been found, then
				-- clarification is now pending.

				-- If find_non_electrical_devices has found only one device
				-- then the flag edit_process_running is set true.
			else
				-- Here the clarification procedure ends.
				-- An object has been selected via procedure clarify_object.
				-- By setting the status of the selected object
				-- as "moving", the selected object
				-- will be drawn according to the given point and 
				-- the tool position.
				set_first_selected_object_moving;

				-- Furtheron, on the next call of this procedure
				-- the selected object will be assigned its final position.
				
				set_edit_process_running;
				reset_request_clarification;
			end if;
			
		else
			-- Finally move the selected device:
			finalize;
		end if;
	end move_object;

	
	

	
	

	
-- ROTATE:


	procedure rotate_object (
		position : in type_vector_model)
	is 

		procedure finalize is
			use et_modes.board;
			use et_undo_redo;
			use et_commit;

			object : type_object := get_first_object (
					active_module, SELECTED, log_threshold + 1);
		begin
			log (text => "finalize rotate", level => log_threshold);
			log_indentation_up;

			-- If a selected object has been found, then
			-- we do the actual finalizing:
			if object.cat /= CAT_VOID then

				reset_status_objects (active_module, log_threshold + 1);
				
				-- Commit the current state of the design:
				commit (PRE, verb, noun, log_threshold + 1);
				
				rotate_object (
					module_cursor	=> active_module, 
					object			=> object, 
					log_threshold	=> log_threshold + 1);

				-- Commit the new state of the design:
				commit (POST, verb, noun, log_threshold + 1);

			else
				log (text => "nothing to do", level => log_threshold);
			end if;
				
			log_indentation_down;			
			
			status_clear;

			reset_editing_process; -- prepare for a new editing process
		end finalize;

		

	begin		
		if not clarification_pending then

			-- Locate all objects in the vicinity of the given point:
			find_objects (position);
			-- NOTE: If many objects have been found, then
			-- clarification is now pending.

			-- If find_objects has found only one object
			-- then the flag edit_process_running is set true.
			if edit_process_running then
			 	finalize;
			end if;
			
		else
			-- Here the clarification procedure ends.
			-- An object has been selected via procedure clarify_object.
			reset_request_clarification;
			finalize;
		end if;
	end rotate_object;






	
	
-- RENAME:


	procedure cb_rename_new_name_entered (
		self : access gtk_entry_record'class) 
	is 
		device_name_new : type_device_name;

		
		-- Renames the selected object:
		procedure finalize is
			use et_modes.board;
			use et_undo_redo;
			use et_commit;

			object : constant type_object := get_first_object (
					active_module, SELECTED, log_threshold + 1);
		begin
			log (text => "finalize rename", level => log_threshold);
			log_indentation_up;

			-- If a selected object has been found, then
			-- we do the actual finalizing:
			if object.cat /= CAT_VOID then

				reset_status_objects (active_module, log_threshold + 1);
				
				-- Commit the current state of the design:
				commit (PRE, verb, noun, log_threshold + 1);
				
				rename_object (
					module_cursor	=> active_module, 
					object			=> object, 
					new_name_device	=> device_name_new,
					log_threshold	=> log_threshold + 1);


				-- Commit the new state of the design:
				commit (POST, verb, noun, log_threshold + 1);

				redraw_board;
				
			else
				log (text => "nothing to do", level => log_threshold);
			end if;
				
			log_indentation_down;			

			reset_editing_process; -- prepare for a new editing process
		end finalize;

		
		
	begin
		device_name_new := to_device_name (self.get_text); -- IC2

		-- CS: Precheck device name ?
		-- put_line ("new name entered: " & to_string (device_name_new));
		
		finalize;

		-- If everything was fine, close the window and clean up.
		-- If one of the operations above has raised an exception then
		-- nothing will be cleaned up and the window will remain until the
		-- operator enters a correct property.
		rename_window.destroy;

		-- CS
		-- Whatever goes wrong, output the message in the status bar
		-- of the properties window:
		-- exception when event: others =>
		-- 	set_status_properties (exception_message (event));
			
	end cb_rename_new_name_entered;




	

	procedure cb_rename_window_destroy (
		window : access gtk_widget_record'class)
	is
	begin
		put_line ("cb_rename_window_destroy");
		reset;
	end cb_rename_window_destroy;


	


	
	procedure show_rename_window is

		-- Get the first selected object (which is a unit):
		object : constant type_object := get_first_object (
				active_module, SELECTED, log_threshold + 1);


		procedure do_it is
			device_name : type_device_name; -- IC1
		begin
			-- Get the name of the selected device:
			device_name := get_device_name (object.non_electrical_device.cursor);
			
			build_rename_window;

			-- Connect the "destroy" signal.
			rename_window.on_destroy (cb_rename_window_destroy'access);

			-- Set the old name of the device in the window:
			rename_old.set_text (to_string (device_name));

			-- Connect the "on_activate" signal (emitted when ENTER pressed)
			-- of the entry field for the new name:
			rename_new.on_activate (cb_rename_new_name_entered'access);
			
			rename_new.grab_focus;
			
			rename_window.show_all;

			rename_window_open := true;			
		end do_it;
		
		
	begin
		case object.cat is
			when CAT_NON_ELECTRICAL_DEVICE =>
				do_it;

			when others =>
				raise constraint_error; -- CS
		end case;
	end show_rename_window;


	

	

	procedure rename_object (
		point : in type_vector_model)
	is begin		
		if not rename_window_open then
		
			if not clarification_pending then
				-- Locate all objects in the vicinity of the given point:
				find_objects (point);
				-- NOTE: If many objects have been found, then
				-- clarification is now pending.

				-- If find_objects has found only one object,				
				-- then delete the object immediateley.
				if edit_process_running then
					show_rename_window;
				end if;
			else
				-- Here the clarification procedure ends.
				-- An object has been selected via procedure clarify_object.

				show_rename_window;
			end if;
		end if;
	end rename_object;




	
	
	

	
-- FLIP / MIRROR:


	procedure flip_object (
		position : in type_vector_model)
	is 

		procedure finalize is
			use et_modes.board;
			use et_undo_redo;
			use et_commit;

			object : type_object := get_first_object (
					active_module, SELECTED, log_threshold + 1);
		begin
			log (text => "finalize flip", level => log_threshold);
			log_indentation_up;

			-- If a selected object has been found, then
			-- we do the actual finalizing:
			if object.cat /= CAT_VOID then

				reset_status_objects (active_module, log_threshold + 1);
				
				-- Commit the current state of the design:
				commit (PRE, verb, noun, log_threshold + 1);
				
				flip_object (
					module_cursor	=> active_module, 
					object			=> object, 
					log_threshold	=> log_threshold + 1);

				-- Commit the new state of the design:
				commit (POST, verb, noun, log_threshold + 1);

			else
				log (text => "nothing to do", level => log_threshold);
			end if;
				
			log_indentation_down;			
			
			status_clear;

			reset_editing_process; -- prepare for a new editing process
		end finalize;

		

	begin		
		if not clarification_pending then

			-- Locate all objects in the vicinity of the given point:
			find_objects (position);
			-- NOTE: If many objects have been found, then
			-- clarification is now pending.

			-- If find_objects has found only one object
			-- then the flag edit_process_running is set true.
			if edit_process_running then
			 	finalize;
			end if;
			
		else
			-- Here the clarification procedure ends.
			-- An object has been selected via procedure clarify_object.
			reset_request_clarification;
			finalize;
		end if;
	end flip_object;

	








	

-- DELETE:	
	

	procedure delete_object (
		position : in type_vector_model)
	is 

		-- Shows some information in the status bar:
		procedure finalize is
			use et_modes.board;
			use et_undo_redo;
			use et_commit;
			
			object : type_object := get_first_object (
					active_module, SELECTED, log_threshold + 1);
		begin
			log (text => "finalize delete", level => log_threshold);
			log_indentation_up;

			-- If a selected object has been found, then
			-- we do the actual finalizing:
			if object.cat /= CAT_VOID then

				reset_status_objects (active_module, log_threshold + 1);
				
				-- Commit the current state of the design:
				commit (PRE, verb, noun, log_threshold + 1);
				
				delete_object (
					module_cursor	=> active_module, 
					object			=> object, 
					log_threshold	=> log_threshold + 1);

				-- Commit the new state of the design:
				commit (POST, verb, noun, log_threshold + 1);

			else
				log (text => "nothing to do", level => log_threshold);
			end if;
				
			log_indentation_down;			
			
			status_clear;

			reset_editing_process; -- prepare for a new editing process
		end finalize;
		

	begin		
		if not clarification_pending then

			-- Locate all objects in the vicinity of the given point:
			find_objects (position);
			-- NOTE: If many objects have been found, then
			-- clarification is now pending.

			-- If find_objects has found only one object
			-- then the flag edit_process_running is set true.
			if edit_process_running then
			 	finalize;
			end if;
			
		else
			-- Here the clarification procedure ends.
			-- An object has been selected via procedure clarify_object.
			reset_request_clarification;
			finalize;
		end if;
	end delete_object;

	


	

-- SHOW:


	procedure show_object (
		position : in type_vector_model)
	is 

		-- Shows some information in the status bar:
		procedure finalize is
			use et_device_property_level;
			use et_devices_electrical.units;
			
			object : type_object := get_first_object (
					active_module, SELECTED, log_threshold + 1);
		begin
			log (text => "finalize show", level => log_threshold);
			log_indentation_up;

			-- If a selected object has been found, then
			-- we do the actual finalizing:
			if object.cat /= CAT_VOID then

				reset_status_objects (active_module, log_threshold + 1);

				-- Show the device:
				show_object (
					module_cursor	=> active_module, 
					object			=> object, 
					log_threshold	=> log_threshold + 1);

				-- Highlight the device in the schematic editor:
				redraw_schematic;


				-- Write some basic information in the status bar:
				case object.cat is
					when CAT_ELECTRICAL_DEVICE =>
						status_clear;
						
						set_status (get_properties (
							device_cursor	=> object.electrical_device.cursor,
							level			=> DEVICE_PROPERTIES_LEVEL_1,						   
							all_units		=> true));

						
					when CAT_NON_ELECTRICAL_DEVICE =>
						status_clear;

						set_status (get_properties (
							device_cursor	=> object.non_electrical_device.cursor,
							level			=> DEVICE_PROPERTIES_LEVEL_1));

						
					when others =>
						status_clear;
						-- CS CAT_PLACEHOLDER ?
				end case;					

				
			else
				log (text => "nothing to do", level => log_threshold);
			end if;
				
			log_indentation_down;			

			reset_editing_process; -- prepare for a new editing process
		end finalize;
		

	begin		
		if not clarification_pending then

			-- Locate all objects in the vicinity of the given point:
			find_objects (position);
			-- NOTE: If many objects have been found, then
			-- clarification is now pending.

			-- If find_objects has found only one object
			-- then the flag edit_process_running is set true.
			if edit_process_running then
			 	finalize;
			end if;
			
		else
			-- Here the clarification procedure ends.
			-- An object has been selected via procedure clarify_object.
			reset_request_clarification;
			finalize;
		end if;
	end show_object;

	
	
end et_canvas_board_devices;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
