------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                           MODULE INSTANCE                                --
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

--   For correct displaying set tab width in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--


with ada.strings.bounded;       	use ada.strings.bounded;

with ada.containers;            	use ada.containers;
with ada.containers.ordered_maps;

with et_module_names;				use et_module_names;
with et_assembly_variant_name;		use et_assembly_variant_name;


package et_module_instance is
	
	

	-- The module instance name is something like LMX_1 or DRV_1. 
	module_instance_name_length_max : constant positive := 20;
	package pac_module_instance_name is new generic_bounded_length (module_instance_name_length_max);

	function to_string (name : in pac_module_instance_name.bounded_string) return string;
	function to_instance_name (name : in string) return pac_module_instance_name.bounded_string;


	
	
	type type_module_instance is record
		generic_name		: pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		assembly_variant	: pac_assembly_variant_name.bounded_string; -- low_cost
		-- CS other properties ?
	end record;


	
	-- Lots of module instances are stored in a map:
	package pac_module_instances is new ordered_maps (
		key_type		=> pac_module_instance_name.bounded_string, -- LMX_1
		"<"				=> pac_module_instance_name."<",
		element_type	=> type_module_instance);

	

end et_module_instance;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
