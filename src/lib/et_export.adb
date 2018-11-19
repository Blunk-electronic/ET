------------------------------------------------------------------------------
--                                                                          --
--                         SYSTEM ET EXPORT                                 --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2018 Mario Blunk, Blunk electronic                 --
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

--   For correct displaying set tab width in your edtior to 4.

--   The two letters "CS" indicate a "construction side" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with et_general;
with et_schematic;
with et_string_processing;			use et_string_processing;

package body et_export is

	procedure create_project_directory (
	-- Creates given project directory in work_directory of ET.
	-- Creates subdirectory for CAM
		project			: in string;
		log_threshold	: in et_string_processing.type_log_level) is
		use et_general;
	begin
		if not exists (compose (work_directory, project)) then
			log ("creating project directory " 
					& compose (work_directory, project) & " ...",
				log_threshold);

			create_directory (compose (work_directory, project));
			create_path (compose (compose (work_directory, project), directory_cam));
		end if;
	end create_project_directory;

end et_export;

-- Soli Deo Gloria
