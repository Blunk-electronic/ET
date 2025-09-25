------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                            DEVICE SECTIONS                               --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2025                                                --
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



package et_device_sections is
	

	-- Prefixes before enumeration types prevent clashes with gnat keywords
	-- and package names:
	section_prefix : constant string := ("SEC_");


	
	type type_device_section is ( -- CS: sort aphabetically
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



	function to_string (
		section : in type_device_section) 
		return string;




	section_symbol				: constant string := "[SYMBOL";
	section_variant				: constant string := "[VARIANT";
	section_variants			: constant string := "[VARIANTS";
	section_terminal_port_map	: constant string := "[TERMINAL_PORT_MAP";

	section_unit				: constant string := "[UNIT";
	section_units_internal		: constant string := "[UNITS_INTERNAL";
	section_units_external		: constant string := "[UNITS_EXTERNAL";

	
	
end et_device_sections;
