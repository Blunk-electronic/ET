------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                          DRAW SELECTED UNIT                              --
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

procedure draw_selected_unit (
	self	: not null access type_view;
	in_area	: in type_rectangle := no_rectangle;
	context : in type_draw_context;
	unit	: in et_schematic_ops.units.type_selected_unit) is

	use et_symbols;
	use et_schematic.type_devices;
	use et_schematic.type_units;
	
	-- The name, value and purpose of the device:
	device_name	: et_devices.type_name; -- like R1, IC100	
	device_value : et_devices.type_value.bounded_string; -- like 100R or TL084
	device_purpose : et_devices.type_purpose.bounded_string; -- like "brightness control"

	-- The number of units provided by the device:
	unit_count	: et_devices.type_unit_count; -- the total number of units

	-- The name of the current unit:
	unit_name	: et_devices.type_unit_name.bounded_string; -- like "I/O Bank 3" or "PWR" or "A" or "B" ...

	-- The placeholders as given by the schematic:
	sch_placeholder_name	: type_text_placeholder (meaning => NAME);
	sch_placeholder_value	: type_text_placeholder (meaning => VALUE);
	sch_placeholder_purpose : type_text_placeholder (meaning => PURPOSE);

	unit_rotation : type_rotation;
	
	-- get the model of the device:
	device_model : et_devices.type_device_model_file.bounded_string :=
		element (unit.device).model;	-- ../libraries/devices/transistor/pnp.dev

	unit_position : type_point; -- only x and y relevant
		
	procedure locate_symbol (unit_cursor : in et_devices.type_unit_cursors) is
		use et_devices;
		use pac_units_external;
		use pac_units_internal;

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

					brightness		=> BRIGHT);
				
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

					brightness		=> BRIGHT);
		end case;
	end locate_symbol;
	
	procedure query_unit is
		use et_devices;
		device_cursor_lib : type_devices.cursor;
	begin
		unit_name := key (unit.unit);
		unit_position := type_point (element (unit.unit).position);
		unit_rotation := rot (element (unit.unit).position);
		--put_line (to_string (unit_name));

		-- Get a copy of the placeholders of the unit:
		if element (unit.unit).appearance = PCB then
			sch_placeholder_name := element (unit.unit).name;
			sch_placeholder_value := element (unit.unit).value;
			sch_placeholder_purpose := element (unit.unit).purpose;
		end if;

		device_cursor_lib := locate_device (device_model);
		unit_count := units_total (device_cursor_lib);
		locate_symbol (locate_unit (device_cursor_lib, unit_name));
	end query_unit;

begin

	-- Get device name, value, purpose:
	if element (unit.device).appearance = PCB then
		device_name := key (unit.device); -- like R1, IC100
		device_value := element (unit.device).value; -- like 100R or TL084
		device_purpose := element (unit.device).purpose; -- like "brightness control"
	end if;

	query_unit;
	
end draw_selected_unit;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
