------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              DEVICE_RW                                   --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 - 2020 Mario Blunk, Blunk electronic          --
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

--   For correct displaying set tab with in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with et_string_processing;
with et_devices;				use et_devices;
with et_symbols;
with et_pcb_stack;

package device_rw is

	keyword_symbol_file			: constant string := "symbol_model";
	keyword_prefix				: constant string := "prefix";		
	
	section_symbol				: constant string := "[SYMBOL";
	section_variant				: constant string := "[VARIANT";
	section_variants			: constant string := "[VARIANTS";
	section_terminal_port_map	: constant string := "[TERMINAL_PORT_MAP";

	section_unit				: constant string := "[UNIT";
	section_units_internal		: constant string := "[UNITS_INTERNAL";
	section_units_external		: constant string := "[UNITS_EXTERNAL";


	
	type type_section is (
		SEC_INIT,
		SEC_VARIANTS,
		SEC_VARIANT,
		SEC_TERMINAL_PORT_MAP,
		SEC_UNITS_INTERNAL,
		SEC_UNIT,
		SEC_SYMBOL,
		SEC_DRAW,
		SEC_LINE,
		SEC_ARC,
		SEC_CIRCLE,
		SEC_TEXTS,
		SEC_TEXT,
		SEC_PLACEHOLDER,		
		SEC_PLACEHOLDERS,
		SEC_PORTS,
		SEC_PORT,
		SEC_UNITS_EXTERNAL
		);

	procedure create_device (
	-- Creates a device and stores it in container et_devices.devices.
		device_name		: in type_device_model_file.bounded_string; -- libraries/devices/7400.dev
		appearance		: in et_symbols.type_appearance;
		log_threshold	: in et_string_processing.type_log_level);
	
	procedure save_device (
		file_name		: in type_device_model_file.bounded_string; -- libraries/devices/7400.dev
		device			: in type_device; -- the actual device model
		log_threshold	: in et_string_processing.type_log_level);

	procedure read_device (
	-- Opens the device and stores it in container et_libraries.devices.
	-- If check_layers.check is YES, then a check will be done that tests
	-- whether all conductor layers are are in 
	-- range type_signal_layer'first .. deepest conductor layer.
		file_name 		: in type_device_model_file.bounded_string; -- ../lbr/logic_ttl/7400.dev
		check_layers	: in et_pcb_stack.type_layer_check := (check => et_pcb_stack.NO);
		log_threshold	: in et_string_processing.type_log_level);

	
end device_rw;
