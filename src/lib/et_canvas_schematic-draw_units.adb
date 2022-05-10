------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              DRAW UNITS                                  --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2022 Mario Blunk, Blunk electronic          --
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
--                                                                          --
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

separate (et_canvas_schematic)

procedure draw_units (
	self	: not null access type_view;
	in_area	: in type_bounding_box := no_area;
	context : in type_draw_context) 
is
	use et_schematic;
	use pac_devices_sch;
	use et_schematic.pac_units;
	
	-- The name, value and purpose of the current device:
	device_name	: et_devices.type_device_name; -- like R1, IC100	
	device_value : pac_device_value.bounded_string; -- like 100R or TL084
	device_purpose : pac_device_purpose.bounded_string; -- like "brightness control"

	-- The number of units provided by the current device:
	unit_count	: et_devices.type_unit_count; -- the total number of units

	-- The name of the current unit:
	unit_name	: et_devices.pac_unit_name.bounded_string; -- like "I/O Bank 3" or "PWR" or "A" or "B" ...

	-- The placeholders as given by the schematic:
	sch_placeholder_name	: et_symbols.type_text_placeholder (meaning => et_symbols.NAME);
	sch_placeholder_value	: et_symbols.type_text_placeholder (meaning => et_symbols.VALUE);
	sch_placeholder_purpose : et_symbols.type_text_placeholder (meaning => et_symbols.PURPOSE);

	unit_rotation : type_rotation;

	
-- 	-- This function returns true if the given placeholder has been moved from the
-- 	-- default position and rotation or if the alignment has been changed:
-- 	function moved_by_operator (placeholder : in et_symbols.type_text_placeholder)
-- 		return boolean is
-- 		use et_symbols;
-- 		use et_text;
-- 		use type et_text.type_text_alignment;
-- 		result : boolean := false;
-- 	begin
-- 		case placeholder.meaning is
-- 			when NAME =>
-- 				if  placeholder.position /= sch_placeholder_name.position or
-- 					placeholder.rotation /= sch_placeholder_name.rotation or
-- 					placeholder.alignment /= sch_placeholder_name.alignment then
-- 					result := true;
-- 				end if;
-- 
-- 			when VALUE =>
-- 				if 	placeholder.position /= sch_placeholder_value.position or
-- 					placeholder.rotation /= sch_placeholder_value.rotation or
-- 					placeholder.alignment /= sch_placeholder_value.alignment then
-- 					result := true;
-- 				end if;
-- 			
-- 			when PURPOSE =>
-- 				if 	placeholder.position /= sch_placeholder_purpose.position or
-- 					placeholder.rotation /= sch_placeholder_purpose.rotation or
-- 					placeholder.alignment /= sch_placeholder_purpose.alignment then
-- 					result := true;
-- 				end if;
-- 		end case;
-- 
-- 		return result;
-- 	end moved_by_operator;

	-- Returns true if the given device matches the device indicated by "selected_unit"
	-- AND if "selected_unit" does not point to particular unit. This way the whole
	-- device is regarded as selected.
	-- Returns true if the given device and unit match the device and unit indicated 
	-- by variable "selected_unit". This way a single unit of a device is regarded as
	-- selected:
	function unit_is_selected (
		d : in pac_devices_sch.cursor;
		u : in et_schematic.pac_units.cursor)
		return boolean
	is
		use pac_proposed_units;
		use et_devices;
		use pac_unit_name;
	begin
		-- If there are no selected units at all, then there is nothing to do:
		if is_empty (proposed_units) then
			return false;
		else
			if selected_unit /= pac_proposed_units.no_element then
				
				-- Compare given device and device name of "selected_unit":
				if key (d) = key (element (selected_unit).device) then

					-- If "selected_unit" does not point to a specific unit
					-- then we regard the whole device as selected:
					if element (selected_unit).unit = et_schematic.pac_units.no_element then
						return true;

					-- If "selected_unit" points to the given unit then the
					-- unit is regarded as selected:
					elsif key (u) = key (element (selected_unit).unit) then
						return true;
						
					else 
						return false;
					end if;
					
				else
					return false;
				end if;
			else
				return false;
			end if;
		end if;

		--exception
		--when event: others =>
			--put_line ("invalid selected unit");
			--return false;
			
	end unit_is_selected;

	-- Returns true if the given placeholder is selected.
	function placeholder_is_selected (
		d : in pac_devices_sch.cursor;
		u : in et_schematic.pac_units.cursor)
		return boolean is
		use pac_proposed_placeholders;
		use et_devices;
		use pac_unit_name;
	begin
		-- If there are no selected placeholders at all, then there is nothing to do:
		if is_empty (proposed_placeholders) then
			return false;
		else
			if selected_placeholder /= pac_proposed_placeholders.no_element then
				-- Compare given device and unit name with selected unit:
				if key (d) = key (element (selected_placeholder).device) and then
					key (u) = key (element (selected_placeholder).unit) then
					-- CS: Improvement: compare cursors directly ?
					
					return true;
				else
					return false;
				end if;
			else
				return false;
			end if;
		end if;
	end placeholder_is_selected;
	
	procedure query_devices (device_cursor : in pac_devices_sch.cursor) is
		
		-- get the model of the current device
		device_model : pac_device_model_file.bounded_string :=
			element (device_cursor).model;	-- ../libraries/devices/transistor/pnp.dev

		unit_position : type_point; -- only x and y relevant

		brightness : type_brightness := NORMAL;
		
		procedure locate_symbol (unit_cursor : in et_devices.type_unit_cursors) is
			use et_devices;
			use pac_units_external;
			use pac_units_internal;

			use et_symbols;
			symbol_model : pac_symbol_model_file.bounded_string; -- like libraries/symbols/NAND.sym
			symbol_cursor : et_symbols.pac_symbols.cursor;
		begin
			case unit_cursor.ext_int is
				when EXT =>
					--put_line ("external unit");
					-- If the unit is external, we must fetch the symbol 
					-- via its model file:
					symbol_model := element (unit_cursor.external).model;
					symbol_cursor := locate (symbol_model);
					draw_symbol (
						self		=> self,
						in_area		=> in_area,
						context		=> context,
						
						symbol		=> pac_symbols.element (symbol_cursor),

						device_name		=> device_name,
						device_value	=> device_value,
						device_purpose	=> device_purpose,

						unit_name		=> unit_name,
						unit_count		=> unit_count,
						
						unit_position	=> unit_position,
						unit_rotation	=> unit_rotation,

						sch_placeholder_name	=> sch_placeholder_name,
						sch_placeholder_value	=> sch_placeholder_value,
						sch_placeholder_purpose => sch_placeholder_purpose,

						brightness		=> brightness
						);
					
				when INT =>
					--put_line ("internal unit");						
					-- If the unit is internal, we can fetch it the symbol 
					-- directly from the unit:
					draw_symbol (
						self		=> self,
						in_area		=> in_area,
						context		=> context,
						
						symbol		=> element (unit_cursor.internal).symbol,

						device_name		=> device_name,
						device_value	=> device_value,
						device_purpose	=> device_purpose,

						unit_name		=> unit_name,
						unit_count		=> unit_count,
						
						unit_position	=> unit_position,
						unit_rotation	=> unit_rotation,

						sch_placeholder_name	=> sch_placeholder_name,
						sch_placeholder_value	=> sch_placeholder_value,
						sch_placeholder_purpose => sch_placeholder_purpose,

						brightness		=> brightness						
						);
			end case;
		end locate_symbol;
		
		procedure query_units (unit_cursor : in et_schematic.pac_units.cursor) is
			use et_devices;
			use et_symbols;
			
			device_cursor_lib : pac_devices_lib.cursor;
		begin
			-- get the name of the unit
			unit_name := key (unit_cursor);
			--put_line (to_string (unit_name));

			-- Get the position of the unit (as it is according to the module database).
			-- If the unit is selected and being moved the the x/y position
			-- will be overwritten by the position of the mouse or the cursor.
			unit_position := element (unit_cursor).position.place;

			-- There are two cases when a unit is to be drawn:
			-- 1. The unit is on the current active sheet.
			-- 2. The unit is being moved from one sheet to another sheet.
			
			-- CASE 1: We draw units which are on the active sheet:
			if get_sheet (element (unit_cursor).position) = current_active_sheet then
				
				-- The default brightness is NORMAL. If the unit is selected then
				-- the brightness will be increased:
				brightness := NORMAL;

				-- CS test verb and noun ?
				if unit_is_selected (device_cursor, unit_cursor) then

					-- increase brightness
					brightness := BRIGHT;

					-- overwrite position
					if unit_move.being_moved then
					
						-- In case the unit is being dragged, backup original position
						-- in global variable "unit_move". Procedure draw_nets requires that
						-- to calculate the displacement of attached net segments:
						unit_move.original_position := unit_position;

						case unit_move.tool is
							when MOUSE =>
								unit_position := self.snap_to_grid (self.mouse_position);
								
							when KEYBOARD =>
								unit_position := cursor_main.position;
						end case;

					end if;
				end if;

				-- get the rotation of the unit
				unit_rotation := get_rotation (element (unit_cursor).position);

				
				-- If this is a real device, then get a copy of the 
				-- placeholders of the unit.
				-- NOTE: The position of the placeholders is relative to
				-- the unit position !
				-- If any of the placeholder of the unit is selected
				-- and being moved, then calculate its new relative
				-- position according to the tool being used. Otherwise
				-- the position remains untouched. 
				-- NOTE: There can only be just one placeholder being moved.
				-- Which one is determined by the selector placeholder.category.
				
				if element (unit_cursor).appearance = PCB then
					sch_placeholder_name := element (unit_cursor).name;
					sch_placeholder_value := element (unit_cursor).value;
					sch_placeholder_purpose := element (unit_cursor).purpose;

					
					if placeholder_is_selected (device_cursor, unit_cursor) then

						-- increase brightness
						brightness := BRIGHT;

						if placeholder_move.being_moved then

							case placeholder_move.category is
								when NAME =>
									
									-- Calculate the absolute position of the NAME placeholder 
									-- as it was according to database BEFORE the move:
									placeholder_move.absolute_position := sch_placeholder_name.position;
									move_by (placeholder_move.absolute_position, to_distance_relative (unit_position));

									-- Depending on the tool used, calculate the new position of the 
									-- placeholder relative to the unit position:
									case placeholder_move.tool is
										when MOUSE =>
											move_by (
												point	=> sch_placeholder_name.position,
												offset	=> get_distance_relative (
													placeholder_move.absolute_position, self.snap_to_grid (self.mouse_position)));

										when KEYBOARD =>
											move_by (
												point	=> sch_placeholder_name.position,
												offset	=> get_distance_relative (
													placeholder_move.absolute_position, cursor_main.position));

									end case;

								when PURPOSE =>

									-- Calculate the absolute position of the PURPOSE placeholder 
									-- as it was according to database BEFORE the move:
									placeholder_move.absolute_position := sch_placeholder_purpose.position;
									move_by (placeholder_move.absolute_position, to_distance_relative (unit_position));

									-- Depending on the tool used, calculate the new position of the 
									-- placeholder relative to the unit position:
									case placeholder_move.tool is
										when MOUSE =>
											move_by (
												point	=> sch_placeholder_purpose.position,
												offset	=> get_distance_relative (
													placeholder_move.absolute_position, self.snap_to_grid (self.mouse_position)));

										when KEYBOARD =>
											move_by (
												point	=> sch_placeholder_purpose.position,
												offset	=> get_distance_relative (
													placeholder_move.absolute_position, cursor_main.position));

									end case;

								when VALUE =>

									-- Calculate the absolute position of the VALUE placeholder 
									-- as it was according to database BEFORE the move:
									placeholder_move.absolute_position := sch_placeholder_value.position;
									move_by (placeholder_move.absolute_position, to_distance_relative (unit_position));

									-- Depending on the tool used, calculate the new position of the 
									-- placeholder relative to the unit position:
									case placeholder_move.tool is
										when MOUSE =>
											move_by (
												point	=> sch_placeholder_value.position,
												offset	=> get_distance_relative (
													placeholder_move.absolute_position, self.snap_to_grid (self.mouse_position)));

										when KEYBOARD =>
											move_by (
												point	=> sch_placeholder_value.position,
												offset	=> get_distance_relative (
													placeholder_move.absolute_position, cursor_main.position));

									end case;

							end case;
									
						end if;
					end if;
				end if;

				device_cursor_lib := locate_device (device_model);
				unit_count := units_total (device_cursor_lib);

				-- locate and draw the symbol:
				locate_symbol (locate_unit (device_cursor_lib, unit_name));
			end if;

			
			-- CASE 2: The unit being moved changes the sheet:
			if unit_move.sheet_changes then
				
				-- CS test verb and noun ?
				if unit_is_selected (device_cursor, unit_cursor) then

					-- increase brightness
					brightness := BRIGHT;

					-- overwrite position
					if unit_move.being_moved then
					
						case unit_move.tool is
							when MOUSE =>
								unit_position := self.snap_to_grid (self.mouse_position);
								
							when KEYBOARD =>
								unit_position := cursor_main.position;
						end case;
					end if;


					-- get the rotation of the unit
					unit_rotation := get_rotation (element (unit_cursor).position);

					
					-- If this is a real device, then get a copy of the 
					-- placeholders of the unit.
					-- NOTE: The position of the placeholders is relative to
					-- the unit position !
					if element (unit_cursor).appearance = PCB then
						sch_placeholder_name := element (unit_cursor).name;
						sch_placeholder_value := element (unit_cursor).value;
						sch_placeholder_purpose := element (unit_cursor).purpose;		
					end if;

					device_cursor_lib := locate_device (device_model);
					unit_count := units_total (device_cursor_lib);

					-- locate and draw the symbol:
					locate_symbol (locate_unit (device_cursor_lib, unit_name));
				end if;
			end if;

		end query_units;

		use et_symbols;
		
	begin -- query_devices

		-- Get device name, value, purpose and number of units of the current device.
		-- Procedure draw_symbol needs them later:
		if element (device_cursor).appearance = PCB then
			device_name := key (device_cursor); -- like R1, IC100
			device_value := element (device_cursor).value; -- like 100R or TL084
			device_purpose := element (device_cursor).purpose; -- like "brightness control"
		end if;
		
		-- Iterate the units of the current device:
		iterate (element (device_cursor).units, query_units'access);
	end query_devices;

	
	-- Draws the unit being added. If there is no unit being added,
	-- then nothing happens here. The unit is drawn in a preview.
	procedure draw_unit_being_added is
		brightness : type_brightness := BRIGHT;
		
		use et_devices;
		use pac_devices_lib;

		procedure locate_symbol (unit_cursor : in et_devices.type_unit_cursors) is
			use pac_units_external;
			use pac_units_internal;

			-- The place where the unit will be drawn.
			-- Depends on the tool used for placing the unit:
			destination : type_point;
			
			use et_symbols;
			use pac_symbols;
			symbol_model : pac_symbol_model_file.bounded_string; -- like libraries/symbols/NAND.sym
			symbol_cursor : et_symbols.pac_symbols.cursor;

			procedure fetch_placeholders_ext is begin
			-- Drawing the symbol of a real device requires placeholders
			-- for name, value an purpose. We fetch them from the symbol model
			-- in this case.
			-- If the symbol is virtual, then the placeholders are meaningless
			-- and assume default values.
				if is_real (symbol_cursor) then
					sch_placeholder_name	:= element (symbol_cursor).name;
					sch_placeholder_value	:= element (symbol_cursor).value;
					sch_placeholder_purpose := element (symbol_cursor).purpose;
				else
					sch_placeholder_name	:= (meaning => NAME, others => <>);
					sch_placeholder_value	:= (meaning => VALUE, others => <>);
					sch_placeholder_purpose := (meaning => PURPOSE, others => <>);
				end if;
			end fetch_placeholders_ext;

			procedure fetch_placeholders_int is begin
			-- Drawing the symbol of a real device requires placeholders
			-- for name, value an purpose. We fetch them from the symbol model
			-- in this case.
			-- If the symbol is virtual, then the placeholders are meaningless
			-- and assume default values.
				case element (unit_cursor.internal).appearance is
					when PCB =>
						sch_placeholder_name	:= element (unit_cursor.internal).symbol.name;
						sch_placeholder_value	:= element (unit_cursor.internal).symbol.value;
						sch_placeholder_purpose := element (unit_cursor.internal).symbol.purpose;
					when VIRTUAL =>
						sch_placeholder_name	:= (meaning => NAME, others => <>);
						sch_placeholder_value	:= (meaning => VALUE, others => <>);
						sch_placeholder_purpose := (meaning => PURPOSE, others => <>);
				end case;
			end fetch_placeholders_int;

			
		begin -- locate_symbol
			-- Set the destination coordinates according to current tool:
			case unit_add.tool is
				when KEYBOARD	=> destination := cursor_main.position;
				when MOUSE		=> destination := self.snap_to_grid (self.mouse_position);
			end case;
			
			case unit_cursor.ext_int is
				when EXT =>
					--put_line ("external unit");
					
					-- If the unit is external, we must fetch the symbol and the placeholders
					-- via its model file:
					symbol_model := element (unit_cursor.external).model;
					symbol_cursor := locate (symbol_model);

					fetch_placeholders_ext;
						
					draw_symbol (
						self		=> self,
						in_area		=> in_area,
						context		=> context,
						
						symbol		=> pac_symbols.element (symbol_cursor),

						device_name		=> unit_add.device_pre,
						unit_name		=> unit_add.name,
						unit_count		=> unit_add.total,
						
						unit_position	=> destination,

						sch_placeholder_name	=> sch_placeholder_name,
						sch_placeholder_value	=> sch_placeholder_value,
						sch_placeholder_purpose => sch_placeholder_purpose,

						brightness		=> brightness,
						preview			=> true);


				when INT =>
					--put_line ("internal unit");						
					
					-- If the unit is internal, we fetch it the symbol and the placeholders 
					-- directly from the unit:
					
					fetch_placeholders_int;
					
					draw_symbol (
						self		=> self,
						in_area		=> in_area,
						context		=> context,
						
						symbol		=> element (unit_cursor.internal).symbol,

						device_name		=> unit_add.device_pre,
						unit_name		=> unit_add.name,
						unit_count		=> unit_add.total,
						
						unit_position	=> destination,

						sch_placeholder_name	=> sch_placeholder_name,
						sch_placeholder_value	=> sch_placeholder_value,
						sch_placeholder_purpose => sch_placeholder_purpose,
						
						brightness		=> brightness,
						preview			=> true);
			end case;
		end locate_symbol;
		
	begin
		-- Once a model has been assigned we know that the unit is to be drawn.
		-- There are two cases when the assigment takes place:

		-- 1. When adding a new device.
		-- The assignment was via procedure et_canvas_schematic_units.add_device.
		-- The unit will be drawn after the first left click or pressing of space key.

		-- 2. When invoking a unit. 
		-- The assignment was via procedure et_canvas_schematic_units.unit_selected.
		
		if unit_add.device /= pac_devices_lib.no_element then

			if activate_counter = 1 -- case #2
			or unit_add.via_invoke then -- case #1
				locate_symbol (locate_unit (unit_add.device, unit_add.name));
			end if;
		end if;

	end draw_unit_being_added;
	
begin
	-- 	put_line ("draw units ...");
	
	iterate (element (current_active_module).devices, query_devices'access);

	-- Draw the unit being added. If no unit is being added, nothing 
	-- happens here:
	draw_unit_being_added;
	
end draw_units;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
