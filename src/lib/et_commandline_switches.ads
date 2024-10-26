------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         COMMANDLINE SWITCHES                             --
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
-- <http://www.gnu.org/licenses/>.   
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

package et_commandline_switches is


-- COMMAND LINE SWITCHES (long switches)
	--switch_about			: constant string (1..7) := "--about"; -- CS
	switch_version					: constant string := "-version";
	switch_log_level				: constant string := "-log-level";
	switch_help						: constant string := "-help";
	switch_make_default_conv		: constant string := "-make-conventions";
	
	switch_import_project			: constant string := "-import-project";
	switch_import_format			: constant string := "-import-format";
	
	switch_native_project_create	: constant string := "-create-project";	
	switch_native_project_open		: constant string := "-open-project";
	switch_native_project_save_as	: constant string := "-save-project-as";
	switch_native_project_module	: constant string := "-module";
	switch_native_project_sheet		: constant string := "-sheet";
	
	switch_native_package_create	: constant string := "-create-package";		
	switch_native_package_open		: constant string := "-open-package";	
	switch_native_package_save_as	: constant string := "-save-package-as";	
	switch_package_appearance		: constant string := "-package-appearance";

	switch_native_symbol_create		: constant string := "-create-symbol";	
	switch_native_symbol_open		: constant string := "-open-symbol";	
	switch_native_symbol_save_as	: constant string := "-save-symbol-as";	
	switch_symbol_appearance		: constant string := "-symbol-appearance";
	
	switch_native_device_create		: constant string := "-create-device";	
	switch_native_device_open		: constant string := "-open-device";	
	switch_native_device_save_as	: constant string := "-save-device-as";	
	switch_device_appearance		: constant string := "-device-appearance";

	switch_frame_schematic_create	: constant string := "-create-schematic-frame";	
	switch_frame_schematic_open		: constant string := "-open-schematic-frame";	
	switch_frame_schematic_save_as	: constant string := "-save-schematic-frame-as";	
	
	switch_frame_pcb_create			: constant string := "-create-pcb-frame";	
	switch_frame_pcb_open			: constant string := "-open-pcb-frame";	
	switch_frame_pcb_save_as		: constant string := "-save-pcb-frame-as";	
	
	switch_execute_script			: constant string := "-script";

	switch_runmode					: constant string := "-runmode";



	
	-- Outputs the command line switches that initiate something.
	procedure show_cdl_switches;

	
	
end et_commandline_switches;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
