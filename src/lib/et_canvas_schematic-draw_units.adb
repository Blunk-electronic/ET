------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              DRAW UNITS                                  --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2020 Mario Blunk, Blunk electronic          --
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
	in_area	: in type_rectangle := no_rectangle;
	context : in type_draw_context) is

	use et_schematic.type_devices;
	use et_schematic.type_units;
	
	-- The name, value and purpose of the current device:
	device_name	: et_devices.type_name; -- like R1, IC100	
	device_value : et_devices.type_value.bounded_string; -- like 100R or TL084
	device_purpose : et_devices.type_purpose.bounded_string; -- like "brightness control"

	-- The number of units provided by the current device:
	unit_count	: et_devices.type_unit_count; -- the total number of units

	-- The name of the current unit:
	unit_name	: et_devices.type_unit_name.bounded_string; -- like "I/O Bank 3" or "PWR" or "A" or "B" ...

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

	-- Returns true if the given unit is selected.
	function unit_is_selected (
		d : in et_schematic.type_devices.cursor;
		u : in et_schematic.type_units.cursor)
		return boolean is
		use pac_proposed_units;
		use et_devices;
		use type_unit_name;
	begin
		-- If there are no selected units at all, then there is nothing to do:
		if is_empty (proposed_units) then
			return false;
		else
			if selected_unit /= pac_proposed_units.no_element then
				-- Compare given device and unit name with selected unit:
				if key (d) = key (element (selected_unit).device) and then
					key (u) = key (element (selected_unit).unit) then
					-- CS: Improvement: compare cursors directly ?
					
					return true;
				else
					return false;
				end if;
			else
				return false;
			end if;
		end if;
	end unit_is_selected;

	-- Returns true if the given placeholder is selected.
	function placeholder_is_selected (
		d : in et_schematic.type_devices.cursor;
		u : in et_schematic.type_units.cursor)
		return boolean is
		use pac_proposed_placeholders;
		use et_devices;
		use type_unit_name;
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
	
	procedure query_devices (device_cursor : in et_schematic.type_devices.cursor) is
		
		-- get the model of the current device
		device_model : et_devices.type_device_model_file.bounded_string :=
			element (device_cursor).model;	-- ../libraries/devices/transistor/pnp.dev

		unit_position : type_point; -- only x and y relevant

		brightness : type_brightness := NORMAL;
		
		procedure locate_symbol (unit_cursor : in et_devices.type_unit_cursors) is
			use et_devices;
			use pac_units_external;
			use pac_units_internal;

			use et_symbols;
			symbol_model : type_symbol_model_file.bounded_string; -- like libraries/symbols/NAND.sym
			symbol_cursor : et_symbols.type_symbols.cursor;
		begin
			case unit_cursor.ext_int is
				when EXT =>
					--put_line ("external unit");
					-- If the unit is external, we must fetch the symbol 
					-- via its model file:
					symbol_model := element (unit_cursor.external).file;
					symbol_cursor := locate (symbol_model);
					draw_symbol (
						self		=> self,
						in_area		=> in_area,
						context		=> context,
						
						symbol		=> type_symbols.element (symbol_cursor),

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
		
		procedure query_units (unit_cursor : in et_schematic.type_units.cursor) is
			use et_devices;
			use et_symbols;
			
			device_cursor_lib : type_devices.cursor;
		begin
			-- we want to draw only those units which are on the active sheet:
			if element (unit_cursor).position.sheet = current_active_sheet then

				-- get the name of the unit
				unit_name := key (unit_cursor);
				--put_line (to_string (unit_name));

				-- Get the position of the unit (as it is according to the module database).
				-- If the unit is selected and being moved the the position
				-- will be overwritten by the position of the mouse or the cursor.
				unit_position := type_point (element (unit_cursor).position);
				
				-- The default brightness is NORMAL. If the unit is selected then
				-- the brightness will be increased:
				brightness := NORMAL;

				-- CS test verb and noun ?
				if unit_is_selected (device_cursor, unit_cursor) then

					-- increase brightness
					brightness := BRIGHT;

					-- overwrite position
					if unit.being_moved then
					
						-- In case the unit is being dragged, backup original position
						-- in global variable "unit". Procedure draw_nets requires that
						-- to calculate the displacement of attached net segments:
						unit.original_position := unit_position;

						case unit.tool is
							when MOUSE =>
								unit_position := self.snap_to_grid (self.mouse_position);
								
							when KEYBOARD =>
								unit_position := cursor_main.position;
						end case;

					end if;
				end if;

				-- get the rotation of the unit
				unit_rotation := rot (element (unit_cursor).position);

				
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

						if placeholder.being_moved then

							case placeholder.category is
								when NAME =>
									
									-- Calculate the absolute position of the NAME placeholder 
									-- as it was according to database BEFORE the move:
									placeholder.absolute_position := sch_placeholder_name.position;
									move_by (placeholder.absolute_position, unit_position);

									-- Depending on the tool used, calculate the new position of the 
									-- placeholder relative to the unit position:
									case placeholder.tool is
										when MOUSE =>
											move_by (
												point	=> sch_placeholder_name.position,
												offset	=> distance_relative (placeholder.absolute_position, self.snap_to_grid (self.mouse_position)));

										when KEYBOARD =>
											move_by (
												point	=> sch_placeholder_name.position,
												offset	=> distance_relative (placeholder.absolute_position, cursor_main.position));

									end case;

								when PURPOSE =>

									-- Calculate the absolute position of the PURPOSE placeholder 
									-- as it was according to database BEFORE the move:
									placeholder.absolute_position := sch_placeholder_purpose.position;
									move_by (placeholder.absolute_position, unit_position);

									-- Depending on the tool used, calculate the new position of the 
									-- placeholder relative to the unit position:
									case placeholder.tool is
										when MOUSE =>
											move_by (
												point	=> sch_placeholder_purpose.position,
												offset	=> distance_relative (placeholder.absolute_position, self.snap_to_grid (self.mouse_position)));

										when KEYBOARD =>
											move_by (
												point	=> sch_placeholder_purpose.position,
												offset	=> distance_relative (placeholder.absolute_position, cursor_main.position));

									end case;

								when VALUE =>

									-- Calculate the absolute position of the VALUE placeholder 
									-- as it was according to database BEFORE the move:
									placeholder.absolute_position := sch_placeholder_value.position;
									move_by (placeholder.absolute_position, unit_position);

									-- Depending on the tool used, calculate the new position of the 
									-- placeholder relative to the unit position:
									case placeholder.tool is
										when MOUSE =>
											move_by (
												point	=> sch_placeholder_value.position,
												offset	=> distance_relative (placeholder.absolute_position, self.snap_to_grid (self.mouse_position)));

										when KEYBOARD =>
											move_by (
												point	=> sch_placeholder_value.position,
												offset	=> distance_relative (placeholder.absolute_position, cursor_main.position));

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
		
-- 		unit_count := et_devices.type_unit_count (length (element (device_cursor).units));

		-- Iterate the units of the current device:
		iterate (element (device_cursor).units, query_units'access);
	end query_devices;
		
begin
	-- 	put_line ("draw units ...");
	
	iterate (element (current_active_module).devices, query_devices'access);
	
end draw_units;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
