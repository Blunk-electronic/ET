------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                              RUNMODE                                     --
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
-- <http://www.gnu.org/licenses/>.   
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

with et_string_processing;		use et_string_processing;
with et_logging;				use et_logging;


package et_runmode is

	
	-- Prefixes before enumeration types prevent clashes with gnat keywords
	-- and package names:	
	runmode_prefix : constant string := "MODE_";

	type type_runmode is (
		MODE_HEADLESS, -- no GUI. commandline only
		MODE_SYMBOL,
		MODE_PACKAGE,
		MODE_DEVICE,
		MODE_MODULE,
		MODE_RIG
		);

	runmode_default : constant type_runmode := MODE_MODULE;

	runmode : type_runmode := runmode_default;
	
	function to_runmode (mode : in string) return type_runmode;
	function to_string (mode : in type_runmode) return string;


	
	procedure skipped_in_this_runmode (log_threshold : in type_log_level);
	
end et_runmode;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
