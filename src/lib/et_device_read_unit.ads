------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                        DEVICE MODEL / READ UNIT                          --
--                                                                          --
--                               S p e c                                    --
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

with et_device_appearance;				use et_device_appearance;
with et_symbol_model;					use et_symbol_model;

with et_device_model_unit_internal;		use et_device_model_unit_internal;
with et_device_model_unit_external;		use et_device_model_unit_external;

with et_string_processing;				use et_string_processing;
with et_logging;						use et_logging;


package et_device_read_unit is


	appearance		: type_appearance;
	

	-- This is the pointer that points to the possible
	-- internal symbol being read in the following:
	symbol_model 	: type_symbol_model_access;


	units_internal	: pac_units_internal.map;
	units_external	: pac_units_external.map;

	
	

-- INTERNAL UNIT:
	
	procedure read_unit_internal (
		line : in type_fields_of_line);


		
	-- Inserts in the temporarily collection of internal units a new unit.
	-- The symbol of the unit is the one accessed by pointer symbol_model.
	procedure insert_unit_internal (
		symbol			: in type_symbol_model_access;
		log_threshold	: in type_log_level);
	
	
	
	
-- EXTERNAL UNIT:
	
	procedure read_unit_external (
		line : in type_fields_of_line);
		
		
	-- Inserts in the temporarily collection of external units a new unit.
	procedure insert_unit_external (
		log_threshold : in type_log_level);

	
end et_device_read_unit;
