------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         DEVICE READ AND WRITE                            --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2024                                                --
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
with et_logging;						use et_logging;
with et_terminals;
with et_devices;						use et_devices;
with et_symbols;
with et_device_appearance;				use et_device_appearance;
with et_pcb_stack;
with et_device_placeholders;			use et_device_placeholders;


package et_device_rw is
	
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

	
	-- Creates adevice and stores it in container et_devices.devices.
	procedure create_device (
		device_name		: in pac_device_model_file.bounded_string; -- libraries/devices/7400.dev
		appearance		: in type_appearance;
		log_threshold	: in type_log_level);


	
	procedure save_device (
		file_name		: in pac_device_model_file.bounded_string; -- libraries/devices/7400.dev
		device			: in type_device_lib; -- the actual device model
		log_threshold	: in type_log_level);

	
	-- Opens the device and stores it in container et_libraries.devices.
	-- If check_layers.check is YES, then a check will be done that tests
	-- whether all conductor layers are are in 
	-- range type_signal_layer'first .. deepest conductor layer.
	procedure read_device (
		file_name 		: in pac_device_model_file.bounded_string; -- ../lbr/logic_ttl/7400.dev
		check_layers	: in et_pcb_stack.type_layer_check := (check => et_pcb_stack.NO);
		log_threshold	: in type_log_level);

	
end et_device_rw;
