------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         COMMANDLINE SWITCHES                             --
--                                                                          --
--                               B o d y                                    --
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

with ada.text_io;					use ada.text_io;

with ada.characters;				use ada.characters;
with ada.characters.latin_1;		use ada.characters.latin_1;


package body et_commandline_switches is


	procedure show_cdl_switches is
		dash : constant character := hyphen;
	begin
		put_line ("available commandline switches:");
		put_line (dash & switch_version);

		put_line (dash & switch_native_project_create);
		put_line (dash & switch_native_project_open);
		put_line (dash & switch_native_project_save_as);

		put_line (dash & switch_native_package_create);
		put_line (dash & switch_native_package_open);		
		put_line (dash & switch_native_package_save_as);

		put_line (dash & switch_native_symbol_create);
		put_line (dash & switch_native_symbol_open);		
		put_line (dash & switch_native_symbol_save_as);

		put_line (dash & switch_native_device_create);
		put_line (dash & switch_native_device_open);		
		put_line (dash & switch_native_device_save_as);

		put_line (dash & switch_frame_schematic_create);
		put_line (dash & switch_frame_schematic_open);		
		put_line (dash & switch_frame_schematic_save_as);

		put_line (dash & switch_frame_pcb_create);
		put_line (dash & switch_frame_pcb_open);		
		put_line (dash & switch_frame_pcb_save_as);
		
		put_line (dash & switch_execute_script);
		put_line (dash & switch_make_default_conv);
		put_line (dash & switch_import_project);

		put_line (dash & switch_runmode);
		
		put_line ("For additional switches and examples see <https://github.com/Blunk-electronic/ET>");
	end show_cdl_switches;



	
end et_commandline_switches;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
