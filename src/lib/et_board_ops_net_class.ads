------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                       BOARD OPERATIONS / NET CLASS                       --
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
--

with et_module_names;				use et_module_names;
with et_generic_modules;			use et_generic_modules;
with et_net_names;					use et_net_names;
with et_nets;						use et_nets;
with et_net_class;					use et_net_class;
with et_net_class_name;				use et_net_class_name;
with et_logging;					use et_logging;


package et_board_ops_net_class is

	use pac_generic_modules;

	

	-- Returns the settings of the required net class
	-- of the given module.
	-- If the given class name is "default" then the settings
	-- are returned as defined by the design rules (DRU).
	-- Assumes that the given class exists for the module.
	-- Otherwise constraint error is raised.
	function get_net_class (
		module	: in pac_generic_modules.cursor; -- the module like motor_driver
		class	: in pac_net_class_name.bounded_string) -- hi-voltage, si-critical
		return type_net_class;



	-- Returns the class settings of a net in a module.
	-- If given net is no_element (freetrack) then the settings of the
	-- "default" class will be returned:
	function get_net_class (
		module	: in pac_generic_modules.cursor; -- the module like motor_driver
		net		: in pac_nets.cursor)  -- GND, RESET_N, ...
		return type_net_class;



	function get_class_name (
		module_cursor	: in pac_generic_modules.cursor;
		net_cursor		: in pac_nets.cursor)  -- GND, RESET_N, ...
		return pac_net_class_name.bounded_string;

	

	-- Sets the net class of a net:
	procedure set_net_class (
		module_cursor	: in pac_generic_modules.cursor;
		net_name		: in pac_net_name.bounded_string; -- RESET, MOTOR_ON_OFF
		net_class		: in pac_net_class_name.bounded_string; -- pwr
		log_threshold	: in type_log_level);



	
end et_board_ops_net_class;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
