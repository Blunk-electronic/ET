------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                           MODULE OPERATIONS                              --
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
--  ToDo: 
--  


with et_generic_modules;		use et_generic_modules;
with et_module_names;			use et_module_names;
with et_logging;				use et_logging;


package et_module_ops is
		

	-- Creates an empty generic module in container modules.
	-- Does not create the actual module file if the module
	-- name is "untitled". If the module name is something other
	-- than "untitled" then the module file will also be created.
	procedure create_module (
		module_name		: in pac_module_name.bounded_string; -- motor_driver, templates/clock_generator
		log_threshold	: in type_log_level);


	
	-- Deletes a generic module (from container generic_modules) and
	-- the module file (*.mod) itself.
	procedure delete_module (
		module_name		: in pac_module_name.bounded_string; -- motor_driver, templates/clock_generator
		log_threshold	: in type_log_level);
		



	
	-- Saves a generic module (from container generic_modules) in a file inside 
	-- the current project directory.
	-- The module must be inside the current project. If it is outside
	-- the project, a warning will be issued and it will NOT be saved.
	-- If the module is outside the project directory then it will not be touched.
	-- If the module does not exist, a warning will be issued.
	procedure save_module (
		module_name		: in pac_module_name.bounded_string; -- motor_driver, templates/clock_generator
		log_threshold	: in type_log_level);
	
	
end et_module_ops;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
