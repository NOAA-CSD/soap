<?xml version='1.0' encoding='UTF-8'?>
<Project Type="Project" LVVersion="13008000">
	<Property Name="NI.LV.All.SourceOnly" Type="Bool">true</Property>
	<Property Name="varPersistentID:{0ACD6376-0F3E-4A80-855D-53C4217B64D4}" Type="Ref">/RT CompactRIO Target/Nick SV.lvlib/NumSamplesPerCycle</Property>
	<Property Name="varPersistentID:{1B6CFF52-E53B-4A59-BEC2-ACE0EBAD5A6B}" Type="Ref">/RT CompactRIO Target/Nick SV.lvlib/DataOut</Property>
	<Property Name="varPersistentID:{69D95DE6-010D-47A0-9A90-3490B0562DC1}" Type="Ref">/RT CompactRIO Target/Nick SV.lvlib/Trim</Property>
	<Property Name="varPersistentID:{70F7639B-F5DA-40DB-820C-193D8DC6A421}" Type="Ref">/RT CompactRIO Target/Nick SV.lvlib/random trace</Property>
	<Property Name="varPersistentID:{96B490E0-4394-41AC-9927-F7377C8336D4}" Type="Ref">/RT CompactRIO Target/Nick SV.lvlib/RepRate</Property>
	<Property Name="varPersistentID:{9F599E80-9792-4E7C-877F-5307BE93BA20}" Type="Ref">/RT CompactRIO Target/Nick SV.lvlib/Nremove</Property>
	<Property Name="varPersistentID:{A92F872C-CF59-49FB-AA36-5D50F13AE004}" Type="Ref">/RT CompactRIO Target/Nick SV.lvlib/CRD Data</Property>
	<Property Name="varPersistentID:{CADE64A0-8307-4111-9C17-2646D870F78F}" Type="Ref">/RT CompactRIO Target/Nick SV.lvlib/Errors</Property>
	<Property Name="varPersistentID:{DF55BC44-669D-4A09-9368-B7D99CFE6BCD}" Type="Ref">/RT CompactRIO Target/Nick SV.lvlib/Stop</Property>
	<Item Name="My Computer" Type="My Computer">
		<Property Name="server.app.propertiesEnabled" Type="Bool">true</Property>
		<Property Name="server.control.propertiesEnabled" Type="Bool">true</Property>
		<Property Name="server.tcp.enabled" Type="Bool">false</Property>
		<Property Name="server.tcp.port" Type="Int">0</Property>
		<Property Name="server.tcp.serviceName" Type="Str">My Computer/VI Server</Property>
		<Property Name="server.tcp.serviceName.default" Type="Str">My Computer/VI Server</Property>
		<Property Name="server.vi.callsEnabled" Type="Bool">true</Property>
		<Property Name="server.vi.propertiesEnabled" Type="Bool">true</Property>
		<Property Name="specify.custom.address" Type="Bool">false</Property>
		<Item Name="For Tim" Type="Folder">
			<Item Name="Tim Front Panel.vi" Type="VI" URL="../Tim Front Panel.vi"/>
		</Item>
		<Item Name="Test" Type="Folder">
			<Item Name="Test Laser Logicv.vi" Type="VI" URL="../Test/Test Laser Logicv.vi"/>
			<Item Name="Test UI.vi" Type="VI" URL="../Test/Test UI.vi"/>
		</Item>
		<Item Name="User Interface" Type="Folder">
			<Item Name="CRD Ringdowns.lvclass" Type="LVClass" URL="../../cPAS/UI/CRDS/Ringdowns/CRD Ringdowns.lvclass"/>
			<Item Name="CRD Subject.lvclass" Type="LVClass" URL="../../cPAS/UI/CRDS/Subject/CRD Subject.lvclass"/>
			<Item Name="Data Subject.lvclass" Type="LVClass" URL="../../../Reusable/Observer/Data Subject.lvclass"/>
			<Item Name="File Observer.lvclass" Type="LVClass" URL="../../../Reusable/File IO/Class/File Observer.lvclass"/>
			<Item Name="GUI Composition.lvclass" Type="LVClass" URL="../../../Reusable/GUI/Composition/GUI Composition.lvclass"/>
			<Item Name="GUI Facade.lvclass" Type="LVClass" URL="../../../Reusable/GUI/Facade/GUI Facade.lvclass"/>
			<Item Name="GUI.lvclass" Type="LVClass" URL="../../../Reusable/GUI/GUI.lvclass"/>
			<Item Name="Main Interface.vi" Type="VI" URL="../UI/Main Interface.vi"/>
			<Item Name="Observer.lvclass" Type="LVClass" URL="../../../Reusable/Observer/Observer.lvclass"/>
			<Item Name="Tim File Observer.lvclass" Type="LVClass" URL="../../Timothy/File Observer/Tim File Observer.lvclass"/>
		</Item>
		<Item Name="File Utilities.lvlib" Type="Library" URL="../../../Reusable/File IO/File Utilities.lvlib"/>
		<Item Name="Nick FP.vi" Type="VI" URL="../Nick FP.vi"/>
		<Item Name="Dependencies" Type="Dependencies">
			<Item Name="user.lib" Type="Folder">
				<Item Name="Session - Root.lvclass" Type="LVClass" URL="/&lt;userlib&gt;/_Session Fwk/Session - Root.lvclass"/>
			</Item>
			<Item Name="vi.lib" Type="Folder">
				<Item Name="Acquire Semaphore.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/semaphor.llb/Acquire Semaphore.vi"/>
				<Item Name="AddNamedSemaphorePrefix.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/semaphor.llb/AddNamedSemaphorePrefix.vi"/>
				<Item Name="BuildHelpPath.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/BuildHelpPath.vi"/>
				<Item Name="Check if File or Folder Exists.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/libraryn.llb/Check if File or Folder Exists.vi"/>
				<Item Name="Check Special Tags.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/Check Special Tags.vi"/>
				<Item Name="Clear Errors.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/Clear Errors.vi"/>
				<Item Name="Convert property node font to graphics font.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/Convert property node font to graphics font.vi"/>
				<Item Name="Details Display Dialog.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/Details Display Dialog.vi"/>
				<Item Name="DialogType.ctl" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/DialogType.ctl"/>
				<Item Name="DialogTypeEnum.ctl" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/DialogTypeEnum.ctl"/>
				<Item Name="DTbl Digital Size.vi" Type="VI" URL="/&lt;vilib&gt;/Waveform/DTblOps.llb/DTbl Digital Size.vi"/>
				<Item Name="DWDT Digital Size.vi" Type="VI" URL="/&lt;vilib&gt;/Waveform/DWDTOps.llb/DWDT Digital Size.vi"/>
				<Item Name="DWDT Get Final Time Value.vi" Type="VI" URL="/&lt;vilib&gt;/Waveform/DWDTOps.llb/DWDT Get Final Time Value.vi"/>
				<Item Name="DWDT Waveform Duration.vi" Type="VI" URL="/&lt;vilib&gt;/Waveform/DWDTOps.llb/DWDT Waveform Duration.vi"/>
				<Item Name="Error Cluster From Error Code.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/Error Cluster From Error Code.vi"/>
				<Item Name="Error Code Database.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/Error Code Database.vi"/>
				<Item Name="ErrWarn.ctl" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/ErrWarn.ctl"/>
				<Item Name="eventvkey.ctl" Type="VI" URL="/&lt;vilib&gt;/event_ctls.llb/eventvkey.ctl"/>
				<Item Name="Find Tag.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/Find Tag.vi"/>
				<Item Name="Format Message String.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/Format Message String.vi"/>
				<Item Name="General Error Handler CORE.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/General Error Handler CORE.vi"/>
				<Item Name="General Error Handler.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/General Error Handler.vi"/>
				<Item Name="Get Final Time Value.vi" Type="VI" URL="/&lt;vilib&gt;/Waveform/WDTOps.llb/Get Final Time Value.vi"/>
				<Item Name="Get LV Class Path.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/LVClass/Get LV Class Path.vi"/>
				<Item Name="Get String Text Bounds.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/Get String Text Bounds.vi"/>
				<Item Name="Get Text Rect.vi" Type="VI" URL="/&lt;vilib&gt;/picture/picture.llb/Get Text Rect.vi"/>
				<Item Name="GetHelpDir.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/GetHelpDir.vi"/>
				<Item Name="GetNamedSemaphorePrefix.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/semaphor.llb/GetNamedSemaphorePrefix.vi"/>
				<Item Name="GetRTHostConnectedProp.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/GetRTHostConnectedProp.vi"/>
				<Item Name="Longest Line Length in Pixels.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/Longest Line Length in Pixels.vi"/>
				<Item Name="LVBoundsTypeDef.ctl" Type="VI" URL="/&lt;vilib&gt;/Utility/miscctls.llb/LVBoundsTypeDef.ctl"/>
				<Item Name="LVDateTimeRec.ctl" Type="VI" URL="/&lt;vilib&gt;/Utility/miscctls.llb/LVDateTimeRec.ctl"/>
				<Item Name="NI_AALPro.lvlib" Type="Library" URL="/&lt;vilib&gt;/Analysis/NI_AALPro.lvlib"/>
				<Item Name="NI_DigFilter_Process.lvlib" Type="Library" URL="/&lt;vilib&gt;/addons/Digital Filter Design/NI_DigFilter_Process.lvlib"/>
				<Item Name="NI_DigFilterLicensed.lvlib" Type="Library" URL="/&lt;vilib&gt;/addons/Digital Filter Design/NI_DigFilterLicensed.lvlib"/>
				<Item Name="NI_FileType.lvlib" Type="Library" URL="/&lt;vilib&gt;/Utility/lvfile.llb/NI_FileType.lvlib"/>
				<Item Name="NI_Gmath.lvlib" Type="Library" URL="/&lt;vilib&gt;/gmath/NI_Gmath.lvlib"/>
				<Item Name="NI_PackedLibraryUtility.lvlib" Type="Library" URL="/&lt;vilib&gt;/Utility/LVLibp/NI_PackedLibraryUtility.lvlib"/>
				<Item Name="Not A Semaphore.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/semaphor.llb/Not A Semaphore.vi"/>
				<Item Name="Not Found Dialog.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/Not Found Dialog.vi"/>
				<Item Name="Number of Waveform Samples.vi" Type="VI" URL="/&lt;vilib&gt;/Waveform/WDTOps.llb/Number of Waveform Samples.vi"/>
				<Item Name="Obtain Semaphore Reference.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/semaphor.llb/Obtain Semaphore Reference.vi"/>
				<Item Name="Release Semaphore Reference.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/semaphor.llb/Release Semaphore Reference.vi"/>
				<Item Name="Release Semaphore.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/semaphor.llb/Release Semaphore.vi"/>
				<Item Name="RemoveNamedSemaphorePrefix.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/semaphor.llb/RemoveNamedSemaphorePrefix.vi"/>
				<Item Name="Search and Replace Pattern.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/Search and Replace Pattern.vi"/>
				<Item Name="Semaphore RefNum" Type="VI" URL="/&lt;vilib&gt;/Utility/semaphor.llb/Semaphore RefNum"/>
				<Item Name="Semaphore Refnum Core.ctl" Type="VI" URL="/&lt;vilib&gt;/Utility/semaphor.llb/Semaphore Refnum Core.ctl"/>
				<Item Name="Set Bold Text.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/Set Bold Text.vi"/>
				<Item Name="Set String Value.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/Set String Value.vi"/>
				<Item Name="Simple Error Handler.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/Simple Error Handler.vi"/>
				<Item Name="Space Constant.vi" Type="VI" URL="/&lt;vilib&gt;/dlg_ctls.llb/Space Constant.vi"/>
				<Item Name="TagReturnType.ctl" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/TagReturnType.ctl"/>
				<Item Name="Three Button Dialog CORE.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/Three Button Dialog CORE.vi"/>
				<Item Name="Three Button Dialog.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/Three Button Dialog.vi"/>
				<Item Name="Trim Whitespace.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/Trim Whitespace.vi"/>
				<Item Name="Validate Semaphore Size.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/semaphor.llb/Validate Semaphore Size.vi"/>
				<Item Name="Version To Dotted String.vi" Type="VI" URL="/&lt;vilib&gt;/_xctls/Version To Dotted String.vi"/>
				<Item Name="Waveform Duration.vi" Type="VI" URL="/&lt;vilib&gt;/Waveform/WDTOps.llb/Waveform Duration.vi"/>
				<Item Name="WDT Get Final Time Value DBL.vi" Type="VI" URL="/&lt;vilib&gt;/Waveform/WDTOps.llb/WDT Get Final Time Value DBL.vi"/>
				<Item Name="WDT Number of Waveform Samples CDB.vi" Type="VI" URL="/&lt;vilib&gt;/Waveform/WDTOps.llb/WDT Number of Waveform Samples CDB.vi"/>
				<Item Name="WDT Number of Waveform Samples DBL.vi" Type="VI" URL="/&lt;vilib&gt;/Waveform/WDTOps.llb/WDT Number of Waveform Samples DBL.vi"/>
				<Item Name="WDT Number of Waveform Samples EXT.vi" Type="VI" URL="/&lt;vilib&gt;/Waveform/WDTOps.llb/WDT Number of Waveform Samples EXT.vi"/>
				<Item Name="WDT Number of Waveform Samples I8.vi" Type="VI" URL="/&lt;vilib&gt;/Waveform/WDTOps.llb/WDT Number of Waveform Samples I8.vi"/>
				<Item Name="WDT Number of Waveform Samples I16.vi" Type="VI" URL="/&lt;vilib&gt;/Waveform/WDTOps.llb/WDT Number of Waveform Samples I16.vi"/>
				<Item Name="WDT Number of Waveform Samples I32.vi" Type="VI" URL="/&lt;vilib&gt;/Waveform/WDTOps.llb/WDT Number of Waveform Samples I32.vi"/>
				<Item Name="WDT Number of Waveform Samples SGL.vi" Type="VI" URL="/&lt;vilib&gt;/Waveform/WDTOps.llb/WDT Number of Waveform Samples SGL.vi"/>
				<Item Name="WDT Waveform Duration DBL.vi" Type="VI" URL="/&lt;vilib&gt;/Waveform/WDTOps.llb/WDT Waveform Duration DBL.vi"/>
				<Item Name="whitespace.ctl" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/whitespace.ctl"/>
				<Item Name="XControlSupport.lvlib" Type="Library" URL="/&lt;vilib&gt;/_xctls/XControlSupport.lvlib"/>
			</Item>
			<Item Name="Airmar Data.lvclass" Type="LVClass" URL="../../../Devices/Airmar/Data/Airmar Data.lvclass"/>
			<Item Name="Alicat Data.lvclass" Type="LVClass" URL="../../../Devices/Alicat/Session Based/Data/Alicat Data.lvclass"/>
			<Item Name="Alicat.lvlib" Type="Library" URL="../../../Devices/Alicat/Alicat.lvlib"/>
			<Item Name="Atmospheric Library.lvlib" Type="Library" URL="../../../Reusable/General/Atmospheric/Atmospheric Library.lvlib"/>
			<Item Name="Base Data Class.lvlib" Type="Library" URL="../../../Reusable/Data/Base Data Class.lvlib"/>
			<Item Name="calData.ctl" Type="VI" URL="../../../Reusable/CRDS/Data/Cell/calData.ctl"/>
			<Item Name="Command.lvclass" Type="LVClass" URL="../../../Reusable/Commands/Command.lvclass"/>
			<Item Name="crd_data_enum.ctl" Type="VI" URL="../../../Reusable/CRDS/Data/Main/crd_data_enum.ctl"/>
			<Item Name="CRDS Cell Data.lvclass" Type="LVClass" URL="../../../Reusable/CRDS/Data/Cell/CRDS Cell Data.lvclass"/>
			<Item Name="CRDS Data.lvclass" Type="LVClass" URL="../../../Reusable/CRDS/Data/Main/CRDS Data.lvclass"/>
			<Item Name="extparam.ctl" Type="VI" URL="../../../Reusable/CRDS/Data/Cell/extparam.ctl"/>
			<Item Name="File Session.lvclass" Type="LVClass" URL="../../../Reusable/Session APIs/File Access/Base/File Session.lvclass"/>
			<Item Name="lvanlys.dll" Type="Document" URL="/&lt;resource&gt;/lvanlys.dll"/>
			<Item Name="LVDFD.dll" Type="Document" URL="/&lt;resource&gt;/LVDFD.dll"/>
			<Item Name="MC Data.lvclass" Type="LVClass" URL="../../../Devices/Alicat/Session Based/Data/MC/MC Data.lvclass"/>
			<Item Name="Parse Model String.vi" Type="VI" URL="../../../Devices/Alicat/Parse Model String.vi"/>
			<Item Name="Ringdown.xctl" Type="XControl" URL="../../cPAS/Interface/CRDS/Controls/Ringdown Xctl/Ringdown.xctl"/>
			<Item Name="Serial IO.lvclass" Type="LVClass" URL="../../../Reusable/Serial IO/Serial IO.lvclass"/>
			<Item Name="tCRD Data.lvclass" Type="LVClass" URL="../../Timothy/CRD/Data/tCRD Data.lvclass"/>
			<Item Name="XDNodeRunTimeDep.lvlib" Type="Library" URL="/&lt;vilib&gt;/Platform/TimedLoop/XDataNode/XDNodeRunTimeDep.lvlib"/>
		</Item>
		<Item Name="Build Specifications" Type="Build">
			<Item Name="Tims App" Type="EXE">
				<Property Name="App_copyErrors" Type="Bool">true</Property>
				<Property Name="App_INI_aliasGUID" Type="Str">{640D00DD-06B1-4818-BDC5-823D9DC4A5E7}</Property>
				<Property Name="App_INI_GUID" Type="Str">{81CD2315-B3C1-4170-8093-665ECCDE355F}</Property>
				<Property Name="App_serverConfig.httpPort" Type="Int">8002</Property>
				<Property Name="App_winsec.description" Type="Str">http://www.NOAA.com</Property>
				<Property Name="Bld_autoIncrement" Type="Bool">true</Property>
				<Property Name="Bld_buildCacheID" Type="Str">{C62591F2-9424-48D5-907B-3C04F72338C5}</Property>
				<Property Name="Bld_buildSpecName" Type="Str">Tims App</Property>
				<Property Name="Bld_excludeInlineSubVIs" Type="Bool">true</Property>
				<Property Name="Bld_excludeLibraryItems" Type="Bool">true</Property>
				<Property Name="Bld_excludePolymorphicVIs" Type="Bool">true</Property>
				<Property Name="Bld_localDestDir" Type="Path">../builds/NI_AB_PROJECTNAME/Tims App</Property>
				<Property Name="Bld_localDestDirType" Type="Str">relativeToCommon</Property>
				<Property Name="Bld_modifyLibraryFile" Type="Bool">true</Property>
				<Property Name="Bld_previewCacheID" Type="Str">{C52DDA96-C751-4613-BCE9-7E18C1EE74B7}</Property>
				<Property Name="Bld_version.build" Type="Int">6</Property>
				<Property Name="Bld_version.major" Type="Int">1</Property>
				<Property Name="Destination[0].destName" Type="Str">Tim.exe</Property>
				<Property Name="Destination[0].path" Type="Path">../builds/NI_AB_PROJECTNAME/Tims App/Tim.exe</Property>
				<Property Name="Destination[0].preserveHierarchy" Type="Bool">true</Property>
				<Property Name="Destination[0].type" Type="Str">App</Property>
				<Property Name="Destination[1].destName" Type="Str">Support Directory</Property>
				<Property Name="Destination[1].path" Type="Path">../builds/NI_AB_PROJECTNAME/Tims App/data</Property>
				<Property Name="DestinationCount" Type="Int">2</Property>
				<Property Name="Source[0].itemID" Type="Str">{F6425D41-A374-4C29-ADD7-F93E62F2FA50}</Property>
				<Property Name="Source[0].type" Type="Str">Container</Property>
				<Property Name="Source[1].destinationIndex" Type="Int">0</Property>
				<Property Name="Source[1].itemID" Type="Ref">/My Computer/For Tim/Tim Front Panel.vi</Property>
				<Property Name="Source[1].sourceInclusion" Type="Str">TopLevel</Property>
				<Property Name="Source[1].type" Type="Str">VI</Property>
				<Property Name="SourceCount" Type="Int">2</Property>
				<Property Name="TgtF_companyName" Type="Str">NOAA</Property>
				<Property Name="TgtF_fileDescription" Type="Str">Tims App</Property>
				<Property Name="TgtF_internalName" Type="Str">Tims App</Property>
				<Property Name="TgtF_legalCopyright" Type="Str">Copyright © 2013 NOAA</Property>
				<Property Name="TgtF_productName" Type="Str">Tims App</Property>
				<Property Name="TgtF_targetfileGUID" Type="Str">{86082017-D453-4B61-9D1B-BC7B39DC7BD8}</Property>
				<Property Name="TgtF_targetfileName" Type="Str">Tim.exe</Property>
			</Item>
			<Item Name="Tims Installer" Type="Installer">
				<Property Name="Destination[0].name" Type="Str">Nicholas</Property>
				<Property Name="Destination[0].parent" Type="Str">{3912416A-D2E5-411B-AFEE-B63654D690C0}</Property>
				<Property Name="Destination[0].tag" Type="Str">{656A0252-B54A-4F52-830D-EDE242E582B6}</Property>
				<Property Name="Destination[0].type" Type="Str">userFolder</Property>
				<Property Name="DestinationCount" Type="Int">1</Property>
				<Property Name="DistPart[0].flavorID" Type="Str">DefaultFull</Property>
				<Property Name="DistPart[0].productID" Type="Str">{CD4CAB41-CAB7-436F-8D66-A83B95F2F3E6}</Property>
				<Property Name="DistPart[0].productName" Type="Str">NI LabVIEW Run-Time Engine 2013 f2</Property>
				<Property Name="DistPart[0].SoftDep[0].exclude" Type="Bool">false</Property>
				<Property Name="DistPart[0].SoftDep[0].productName" Type="Str">NI LabVIEW 2013 Run-Time Engine Non-English Support.</Property>
				<Property Name="DistPart[0].SoftDep[0].upgradeCode" Type="Str">{DDE96050-C9ED-4EAC-B310-5EA217776FEA}</Property>
				<Property Name="DistPart[0].SoftDep[1].exclude" Type="Bool">false</Property>
				<Property Name="DistPart[0].SoftDep[1].productName" Type="Str">NI ActiveX Container</Property>
				<Property Name="DistPart[0].SoftDep[1].upgradeCode" Type="Str">{1038A887-23E1-4289-B0BD-0C4B83C6BA21}</Property>
				<Property Name="DistPart[0].SoftDep[2].exclude" Type="Bool">false</Property>
				<Property Name="DistPart[0].SoftDep[2].productName" Type="Str">NI System Web Server 13.0</Property>
				<Property Name="DistPart[0].SoftDep[2].upgradeCode" Type="Str">{FCF64B73-B7D4-4971-8F11-24BAF7CC3E6C}</Property>
				<Property Name="DistPart[0].SoftDep[3].exclude" Type="Bool">false</Property>
				<Property Name="DistPart[0].SoftDep[3].productName" Type="Str">Math Kernel Libraries</Property>
				<Property Name="DistPart[0].SoftDep[3].upgradeCode" Type="Str">{EA05DCF9-F239-4387-8904-04D678031F6B}</Property>
				<Property Name="DistPart[0].SoftDep[4].exclude" Type="Bool">false</Property>
				<Property Name="DistPart[0].SoftDep[4].productName" Type="Str">NI Logos 5.5</Property>
				<Property Name="DistPart[0].SoftDep[4].upgradeCode" Type="Str">{5E4A4CE3-4D06-11D4-8B22-006008C16337}</Property>
				<Property Name="DistPart[0].SoftDep[5].exclude" Type="Bool">false</Property>
				<Property Name="DistPart[0].SoftDep[5].productName" Type="Str">NI TDM Streaming 2.4</Property>
				<Property Name="DistPart[0].SoftDep[5].upgradeCode" Type="Str">{4CD11BE6-6BB7-4082-8A27-C13771BC309B}</Property>
				<Property Name="DistPart[0].SoftDep[6].exclude" Type="Bool">false</Property>
				<Property Name="DistPart[0].SoftDep[6].productName" Type="Str">NI LabVIEW 2013 Web Server</Property>
				<Property Name="DistPart[0].SoftDep[6].upgradeCode" Type="Str">{A66A5CC8-EEB5-4982-8B92-F591AF36060E}</Property>
				<Property Name="DistPart[0].SoftDep[7].exclude" Type="Bool">false</Property>
				<Property Name="DistPart[0].SoftDep[7].productName" Type="Str">NI LabVIEW 2013 Real-Time NBFifo</Property>
				<Property Name="DistPart[0].SoftDep[7].upgradeCode" Type="Str">{76E4763E-0DC4-4195-BC50-084193FFA84B}</Property>
				<Property Name="DistPart[0].SoftDep[8].exclude" Type="Bool">false</Property>
				<Property Name="DistPart[0].SoftDep[8].productName" Type="Str">NI VC2008MSMs</Property>
				<Property Name="DistPart[0].SoftDep[8].upgradeCode" Type="Str">{FDA3F8BB-BAA9-45D7-8DC7-22E1F5C76315}</Property>
				<Property Name="DistPart[0].SoftDep[9].exclude" Type="Bool">false</Property>
				<Property Name="DistPart[0].SoftDep[9].productName" Type="Str">NI Error Reporting 2013</Property>
				<Property Name="DistPart[0].SoftDep[9].upgradeCode" Type="Str">{42E818C6-2B08-4DE7-BD91-B0FD704C119A}</Property>
				<Property Name="DistPart[0].SoftDepCount" Type="Int">10</Property>
				<Property Name="DistPart[0].upgradeCode" Type="Str">{A661D981-0A3B-4081-A5B5-1FAA03CFAF84}</Property>
				<Property Name="DistPartCount" Type="Int">1</Property>
				<Property Name="INST_author" Type="Str">NOAA</Property>
				<Property Name="INST_autoIncrement" Type="Bool">true</Property>
				<Property Name="INST_buildLocation" Type="Path">../builds/Timothy/Tims Installer</Property>
				<Property Name="INST_buildLocation.type" Type="Str">relativeToCommon</Property>
				<Property Name="INST_buildSpecName" Type="Str">Tims Installer</Property>
				<Property Name="INST_defaultDir" Type="Str">{656A0252-B54A-4F52-830D-EDE242E582B6}</Property>
				<Property Name="INST_productName" Type="Str">Timothy</Property>
				<Property Name="INST_productVersion" Type="Str">1.0.7</Property>
				<Property Name="InstSpecBitness" Type="Str">32-bit</Property>
				<Property Name="InstSpecVersion" Type="Str">13008051</Property>
				<Property Name="MSI_arpCompany" Type="Str">NOAA</Property>
				<Property Name="MSI_arpURL" Type="Str">http://www.noaa.com/</Property>
				<Property Name="MSI_distID" Type="Str">{A20FA3FA-0E14-4588-90B1-0BBF1A635182}</Property>
				<Property Name="MSI_osCheck" Type="Int">0</Property>
				<Property Name="MSI_upgradeCode" Type="Str">{D764BC19-C0F2-49DD-BB80-19FBE367129C}</Property>
				<Property Name="RegDest[0].dirName" Type="Str">Software</Property>
				<Property Name="RegDest[0].dirTag" Type="Str">{DDFAFC8B-E728-4AC8-96DE-B920EBB97A86}</Property>
				<Property Name="RegDest[0].parentTag" Type="Str">2</Property>
				<Property Name="RegDestCount" Type="Int">1</Property>
				<Property Name="Source[0].dest" Type="Str">{656A0252-B54A-4F52-830D-EDE242E582B6}</Property>
				<Property Name="Source[0].File[0].dest" Type="Str">{656A0252-B54A-4F52-830D-EDE242E582B6}</Property>
				<Property Name="Source[0].File[0].name" Type="Str">Tim.exe</Property>
				<Property Name="Source[0].File[0].Shortcut[0].destIndex" Type="Int">0</Property>
				<Property Name="Source[0].File[0].Shortcut[0].name" Type="Str">Tim</Property>
				<Property Name="Source[0].File[0].Shortcut[0].subDir" Type="Str">Nicholas</Property>
				<Property Name="Source[0].File[0].ShortcutCount" Type="Int">1</Property>
				<Property Name="Source[0].File[0].tag" Type="Str">{86082017-D453-4B61-9D1B-BC7B39DC7BD8}</Property>
				<Property Name="Source[0].FileCount" Type="Int">1</Property>
				<Property Name="Source[0].name" Type="Str">Tims App</Property>
				<Property Name="Source[0].tag" Type="Ref">/My Computer/Build Specifications/Tims App</Property>
				<Property Name="Source[0].type" Type="Str">EXE</Property>
				<Property Name="SourceCount" Type="Int">1</Property>
			</Item>
			<Item Name="Tims Installer no RTE" Type="Installer">
				<Property Name="Destination[0].name" Type="Str">Nicholas</Property>
				<Property Name="Destination[0].parent" Type="Str">{3912416A-D2E5-411B-AFEE-B63654D690C0}</Property>
				<Property Name="Destination[0].tag" Type="Str">{656A0252-B54A-4F52-830D-EDE242E582B6}</Property>
				<Property Name="Destination[0].type" Type="Str">userFolder</Property>
				<Property Name="DestinationCount" Type="Int">1</Property>
				<Property Name="INST_author" Type="Str">NOAA</Property>
				<Property Name="INST_autoIncrement" Type="Bool">true</Property>
				<Property Name="INST_buildLocation" Type="Path">../builds/Timothy no RTE/Tims Installer</Property>
				<Property Name="INST_buildLocation.type" Type="Str">relativeToCommon</Property>
				<Property Name="INST_buildSpecName" Type="Str">Tims Installer no RTE</Property>
				<Property Name="INST_defaultDir" Type="Str">{656A0252-B54A-4F52-830D-EDE242E582B6}</Property>
				<Property Name="INST_productName" Type="Str">Timothy no RTE</Property>
				<Property Name="INST_productVersion" Type="Str">1.0.8</Property>
				<Property Name="InstSpecBitness" Type="Str">32-bit</Property>
				<Property Name="InstSpecVersion" Type="Str">13008051</Property>
				<Property Name="MSI_arpCompany" Type="Str">NOAA</Property>
				<Property Name="MSI_arpURL" Type="Str">http://www.noaa.com/</Property>
				<Property Name="MSI_distID" Type="Str">{B40DC734-4B62-4B8D-9860-DC6E6EC3F766}</Property>
				<Property Name="MSI_osCheck" Type="Int">0</Property>
				<Property Name="MSI_upgradeCode" Type="Str">{641CE0EA-FBBF-45CE-8FE0-46FB8C4D3DB8}</Property>
				<Property Name="RegDest[0].dirName" Type="Str">Software</Property>
				<Property Name="RegDest[0].dirTag" Type="Str">{DDFAFC8B-E728-4AC8-96DE-B920EBB97A86}</Property>
				<Property Name="RegDest[0].parentTag" Type="Str">2</Property>
				<Property Name="RegDestCount" Type="Int">1</Property>
				<Property Name="Source[0].dest" Type="Str">{656A0252-B54A-4F52-830D-EDE242E582B6}</Property>
				<Property Name="Source[0].File[0].dest" Type="Str">{656A0252-B54A-4F52-830D-EDE242E582B6}</Property>
				<Property Name="Source[0].File[0].name" Type="Str">Tim.exe</Property>
				<Property Name="Source[0].File[0].Shortcut[0].destIndex" Type="Int">0</Property>
				<Property Name="Source[0].File[0].Shortcut[0].name" Type="Str">Tim</Property>
				<Property Name="Source[0].File[0].Shortcut[0].subDir" Type="Str">Nicholas</Property>
				<Property Name="Source[0].File[0].ShortcutCount" Type="Int">1</Property>
				<Property Name="Source[0].File[0].tag" Type="Str">{86082017-D453-4B61-9D1B-BC7B39DC7BD8}</Property>
				<Property Name="Source[0].FileCount" Type="Int">1</Property>
				<Property Name="Source[0].name" Type="Str">Tims App</Property>
				<Property Name="Source[0].tag" Type="Ref">/My Computer/Build Specifications/Tims App</Property>
				<Property Name="Source[0].type" Type="Str">EXE</Property>
				<Property Name="SourceCount" Type="Int">1</Property>
			</Item>
		</Item>
	</Item>
	<Item Name="RT CompactRIO Target" Type="RT CompactRIO">
		<Property Name="alias.name" Type="Str">RT CompactRIO Target</Property>
		<Property Name="alias.value" Type="Str">10.172.240.107</Property>
		<Property Name="CCSymbols" Type="Str">OS,PharLap;CPU,x86;DeviceCode,753D;TARGET_TYPE,RT;</Property>
		<Property Name="crio.ControllerPID" Type="Str">753D</Property>
		<Property Name="crio.family" Type="Str">900x</Property>
		<Property Name="DisableAutoDeployVariables" Type="Bool">true</Property>
		<Property Name="host.ResponsivenessCheckEnabled" Type="Bool">true</Property>
		<Property Name="host.ResponsivenessCheckPingDelay" Type="UInt">5000</Property>
		<Property Name="host.ResponsivenessCheckPingTimeout" Type="UInt">1000</Property>
		<Property Name="host.TargetCPUID" Type="UInt">3</Property>
		<Property Name="host.TargetOSID" Type="UInt">15</Property>
		<Property Name="target.cleanupVisa" Type="Bool">false</Property>
		<Property Name="target.FPProtocolGlobals_ControlTimeLimit" Type="Int">300</Property>
		<Property Name="target.getDefault-&gt;WebServer.Port" Type="Int">80</Property>
		<Property Name="target.getDefault-&gt;WebServer.Timeout" Type="Int">60</Property>
		<Property Name="target.IOScan.Faults" Type="Str">1.0,1;-66460,0,0,0,0;</Property>
		<Property Name="target.IOScan.NetVarPeriod" Type="UInt">100</Property>
		<Property Name="target.IOScan.NetWatchdogEnabled" Type="Bool">false</Property>
		<Property Name="target.IOScan.Period" Type="UInt">10000</Property>
		<Property Name="target.IOScan.PowerupMode" Type="UInt">0</Property>
		<Property Name="target.IOScan.Priority" Type="UInt">0</Property>
		<Property Name="target.IOScan.ReportModeConflict" Type="Bool">true</Property>
		<Property Name="target.IOScan.StartEngineOnDeploy" Type="Bool">false</Property>
		<Property Name="target.IsRemotePanelSupported" Type="Bool">true</Property>
		<Property Name="target.RTCPULoadMonitoringEnabled" Type="Bool">true</Property>
		<Property Name="target.RTDebugWebServerHTTPPort" Type="UInt">8001</Property>
		<Property Name="target.RTTarget.ApplicationPath" Type="Path">/c/ni-rt/startup/startup.rtexe</Property>
		<Property Name="target.RTTarget.EnableFileSharing" Type="Bool">true</Property>
		<Property Name="target.RTTarget.IPAccess" Type="Str">+*</Property>
		<Property Name="target.RTTarget.LaunchAppAtBoot" Type="Bool">false</Property>
		<Property Name="target.RTTarget.VIPath" Type="Path">/c/ni-rt/startup</Property>
		<Property Name="target.server.app.propertiesEnabled" Type="Bool">true</Property>
		<Property Name="target.server.control.propertiesEnabled" Type="Bool">true</Property>
		<Property Name="target.server.tcp.access" Type="Str">+*</Property>
		<Property Name="target.server.tcp.enabled" Type="Bool">false</Property>
		<Property Name="target.server.tcp.paranoid" Type="Bool">true</Property>
		<Property Name="target.server.tcp.port" Type="Int">3363</Property>
		<Property Name="target.server.tcp.serviceName" Type="Str"></Property>
		<Property Name="target.server.tcp.serviceName.default" Type="Str">Main Application Instance/VI Server</Property>
		<Property Name="target.server.vi.access" Type="Str">+*</Property>
		<Property Name="target.server.vi.callsEnabled" Type="Bool">true</Property>
		<Property Name="target.server.vi.propertiesEnabled" Type="Bool">true</Property>
		<Property Name="target.server.viscripting.showScriptingOperationsInContextHelp" Type="Bool">false</Property>
		<Property Name="target.server.viscripting.showScriptingOperationsInEditor" Type="Bool">false</Property>
		<Property Name="target.WebServer.Config" Type="Str"># Web server configuration file.
# Generated by LabVIEW 13.0f2
# 1/24/2014 9:05:34 AM

#
# Global Directives
#
NI.AddLVRouteVars
TypesConfig "$LVSERVER_CONFIGROOT/mime.types"
LimitWorkers 10
LoadModulePath "$LVSERVER_MODULEPATHS"
LoadModule LVAuth lvauthmodule
LoadModule LVRFP lvrfpmodule
Listen 8000

#
# Directives that apply to the default server
#
NI.ServerName default
DocumentRoot "$LVSERVER_DOCROOT"
InactivityTimeout 60
SetConnector netConnector
AddHandler LVAuth
AddHandler LVRFP
AddHandler fileHandler ""
AddOutputFilter chunkFilter
DirectoryIndex index.htm
</Property>
		<Property Name="target.WebServer.Enabled" Type="Bool">false</Property>
		<Property Name="target.WebServer.LogEnabled" Type="Bool">false</Property>
		<Property Name="target.WebServer.LogPath" Type="Path">/c/ni-rt/system/www/www.log</Property>
		<Property Name="target.WebServer.Port" Type="Int">80</Property>
		<Property Name="target.WebServer.RootPath" Type="Path">/c/ni-rt/system/www</Property>
		<Property Name="target.WebServer.TcpAccess" Type="Str">c+*</Property>
		<Property Name="target.WebServer.Timeout" Type="Int">60</Property>
		<Property Name="target.WebServer.ViAccess" Type="Str">+*</Property>
		<Property Name="target.webservices.SecurityAPIKey" Type="Str">PqVr/ifkAQh+lVrdPIykXlFvg12GhhQFR8H9cUhphgg=:pTe9HRlQuMfJxAG6QCGq7UvoUpJzAzWGKy5SbZ+roSU=</Property>
		<Property Name="target.webservices.ValidTimestampWindow" Type="Int">15</Property>
		<Item Name="Root Device" Type="Folder">
			<Item Name="Devices" Type="Folder">
				<Item Name="Device.lvclass" Type="LVClass" URL="../../../Devices/Root/Device.lvclass"/>
				<Item Name="Vaisala Probe.lvclass" Type="LVClass" URL="../../../Devices/Vaisala Probe/Vaisala Probe.lvclass"/>
			</Item>
			<Item Name="Resources" Type="Folder">
				<Item Name="Resource.lvclass" Type="LVClass" URL="../../../Reusable/Resources/Root/Resource.lvclass"/>
				<Item Name="Serial.lvclass" Type="LVClass" URL="../../../Reusable/Resources/Serial/Serial.lvclass"/>
			</Item>
		</Item>
		<Item Name="Actor Framework.lvlib" Type="Library" URL="/&lt;vilib&gt;/ActorFramework/Actor Framework.lvlib"/>
		<Item Name="Chassis" Type="cRIO Chassis">
			<Property Name="crio.ProgrammingMode" Type="Str">express</Property>
			<Property Name="crio.ResourceID" Type="Str">RIO0</Property>
			<Property Name="crio.Type" Type="Str">cRIO-9081</Property>
			<Property Name="NI.SortType" Type="Int">3</Property>
			<Item Name="Mod7" Type="RIO C Series Module">
				<Property Name="crio.Calibration" Type="Str">1</Property>
				<Property Name="crio.Location" Type="Str">Slot 7</Property>
				<Property Name="crio.RequiresValidation" Type="Bool">false</Property>
				<Property Name="crio.SDcounterSlaveChannelMask" Type="Str">0</Property>
				<Property Name="crio.SDCounterSlaveMasterSlot" Type="Str">0</Property>
				<Property Name="crio.SDInputFilter" Type="Str">128</Property>
				<Property Name="crio.SupportsDynamicRes" Type="Bool">false</Property>
				<Property Name="crio.Type" Type="Str">NI 9871</Property>
				<Property Name="cRIOModule.DigitalIOMode" Type="Str">0</Property>
				<Property Name="cRIOModule.EnableSpecialtyDigital" Type="Str">false</Property>
				<Property Name="cRIOModule.kBaudRateDivider1" Type="Str">384</Property>
				<Property Name="cRIOModule.kBaudRateDivider2" Type="Str">384</Property>
				<Property Name="cRIOModule.kBaudRateDivider3" Type="Str">384</Property>
				<Property Name="cRIOModule.kBaudRateDivider4" Type="Str">384</Property>
				<Property Name="cRIOModule.kBaudRatePrescaler1" Type="Str">1</Property>
				<Property Name="cRIOModule.kBaudRatePrescaler2" Type="Str">1</Property>
				<Property Name="cRIOModule.kBaudRatePrescaler3" Type="Str">1</Property>
				<Property Name="cRIOModule.kBaudRatePrescaler4" Type="Str">1</Property>
				<Property Name="cRIOModule.kDataBits1" Type="Str">4</Property>
				<Property Name="cRIOModule.kDataBits2" Type="Str">4</Property>
				<Property Name="cRIOModule.kDataBits3" Type="Str">4</Property>
				<Property Name="cRIOModule.kDataBits4" Type="Str">4</Property>
				<Property Name="cRIOModule.kDesiredBaudRate1" Type="Str">9.600000E+3</Property>
				<Property Name="cRIOModule.kDesiredBaudRate2" Type="Str">9.600000E+3</Property>
				<Property Name="cRIOModule.kDesiredBaudRate3" Type="Str">9.600000E+3</Property>
				<Property Name="cRIOModule.kDesiredBaudRate4" Type="Str">9.600000E+3</Property>
				<Property Name="cRIOModule.kFlowControl1" Type="Str">1</Property>
				<Property Name="cRIOModule.kFlowControl2" Type="Str">1</Property>
				<Property Name="cRIOModule.kFlowControl3" Type="Str">1</Property>
				<Property Name="cRIOModule.kFlowControl4" Type="Str">1</Property>
				<Property Name="cRIOModule.kParity1" Type="Str">1</Property>
				<Property Name="cRIOModule.kParity2" Type="Str">1</Property>
				<Property Name="cRIOModule.kParity3" Type="Str">1</Property>
				<Property Name="cRIOModule.kParity4" Type="Str">1</Property>
				<Property Name="cRIOModule.kStopBits1" Type="Str">1</Property>
				<Property Name="cRIOModule.kStopBits2" Type="Str">1</Property>
				<Property Name="cRIOModule.kStopBits3" Type="Str">1</Property>
				<Property Name="cRIOModule.kStopBits4" Type="Str">1</Property>
				<Property Name="cRIOModule.kXcvrMode1" Type="Str">"00"</Property>
				<Property Name="cRIOModule.kXcvrMode2" Type="Str">"00"</Property>
				<Property Name="cRIOModule.kXcvrMode3" Type="Str">"00"</Property>
				<Property Name="cRIOModule.kXcvrMode4" Type="Str">"00"</Property>
				<Item Name="Port1" Type="RIO Subresource"/>
				<Item Name="Port2" Type="RIO Subresource"/>
				<Item Name="Port3" Type="RIO Subresource"/>
				<Item Name="Port4" Type="RIO Subresource"/>
			</Item>
		</Item>
		<Item Name="CRDS Cell.lvclass" Type="LVClass" URL="../../../Reusable/CRDS/Cell/CRDS Cell.lvclass"/>
		<Item Name="CRDS.lvclass" Type="LVClass" URL="../../../Reusable/CRDS/Main/CRDS.lvclass"/>
		<Item Name="Exponential Fit Library.lvlib" Type="Library" URL="../../../Reusable/General/Math/Fit Exponential Class/Exponential Fit Library.lvlib"/>
		<Item Name="FPGA Ref.ctl" Type="VI" URL="../FPGA Ref.ctl"/>
		<Item Name="Instrument.lvclass" Type="LVClass" URL="../../../Reusable/Instrument/Instrument.lvclass"/>
		<Item Name="nCRD.lvclass" Type="LVClass" URL="../CRD/nCRD.lvclass"/>
		<Item Name="Nick SV.lvlib" Type="Library" URL="../Nick SV.lvlib"/>
		<Item Name="Server.vi" Type="VI" URL="../Server.vi"/>
		<Item Name="Test RD Retrieval - Target - Nick.vi" Type="VI" URL="../Test/Test RD Retrieval - Target - Nick.vi"/>
		<Item Name="Test RD Retrieval - Target.vi" Type="VI" URL="../Test/Test RD Retrieval - Target.vi"/>
		<Item Name="Test Serial.vi" Type="VI" URL="../Test/Test Serial.vi"/>
		<Item Name="Tim Server.vi" Type="VI" URL="../Tim Server.vi"/>
		<Item Name="Dependencies" Type="Dependencies">
			<Item Name="user.lib" Type="Folder">
				<Item Name="Session - Root.lvclass" Type="LVClass" URL="/&lt;userlib&gt;/_Session Fwk/Session - Root.lvclass"/>
				<Item Name="Tick Count (ms)__ogtk.vi" Type="VI" URL="/&lt;userlib&gt;/_OpenG.lib/time/time.llb/Tick Count (ms)__ogtk.vi"/>
			</Item>
			<Item Name="vi.lib" Type="Folder">
				<Item Name="Acquire Semaphore.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/semaphor.llb/Acquire Semaphore.vi"/>
				<Item Name="AddNamedSemaphorePrefix.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/semaphor.llb/AddNamedSemaphorePrefix.vi"/>
				<Item Name="BuildHelpPath.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/BuildHelpPath.vi"/>
				<Item Name="Check if File or Folder Exists.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/libraryn.llb/Check if File or Folder Exists.vi"/>
				<Item Name="Check Special Tags.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/Check Special Tags.vi"/>
				<Item Name="Clear Errors.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/Clear Errors.vi"/>
				<Item Name="Convert property node font to graphics font.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/Convert property node font to graphics font.vi"/>
				<Item Name="Details Display Dialog.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/Details Display Dialog.vi"/>
				<Item Name="dfd_Mutate Filter Obj QSetting 0x100 to 0x200.vi" Type="VI" URL="/&lt;vilib&gt;/addons/Digital Filter Design/_Objects.llb/dfd_Mutate Filter Obj QSetting 0x100 to 0x200.vi"/>
				<Item Name="DialogType.ctl" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/DialogType.ctl"/>
				<Item Name="DialogTypeEnum.ctl" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/DialogTypeEnum.ctl"/>
				<Item Name="DTbl Digital Size.vi" Type="VI" URL="/&lt;vilib&gt;/Waveform/DTblOps.llb/DTbl Digital Size.vi"/>
				<Item Name="DWDT Digital Size.vi" Type="VI" URL="/&lt;vilib&gt;/Waveform/DWDTOps.llb/DWDT Digital Size.vi"/>
				<Item Name="DWDT Get Final Time Value.vi" Type="VI" URL="/&lt;vilib&gt;/Waveform/DWDTOps.llb/DWDT Get Final Time Value.vi"/>
				<Item Name="DWDT Waveform Duration.vi" Type="VI" URL="/&lt;vilib&gt;/Waveform/DWDTOps.llb/DWDT Waveform Duration.vi"/>
				<Item Name="Error Cluster From Error Code.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/Error Cluster From Error Code.vi"/>
				<Item Name="Error Code Database.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/Error Code Database.vi"/>
				<Item Name="ErrWarn.ctl" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/ErrWarn.ctl"/>
				<Item Name="eventvkey.ctl" Type="VI" URL="/&lt;vilib&gt;/event_ctls.llb/eventvkey.ctl"/>
				<Item Name="Find First Error.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/Find First Error.vi"/>
				<Item Name="Find Tag.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/Find Tag.vi"/>
				<Item Name="Format Message String.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/Format Message String.vi"/>
				<Item Name="General Error Handler CORE.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/General Error Handler CORE.vi"/>
				<Item Name="General Error Handler.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/General Error Handler.vi"/>
				<Item Name="Get Final Time Value.vi" Type="VI" URL="/&lt;vilib&gt;/Waveform/WDTOps.llb/Get Final Time Value.vi"/>
				<Item Name="Get String Text Bounds.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/Get String Text Bounds.vi"/>
				<Item Name="Get Text Rect.vi" Type="VI" URL="/&lt;vilib&gt;/picture/picture.llb/Get Text Rect.vi"/>
				<Item Name="GetHelpDir.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/GetHelpDir.vi"/>
				<Item Name="GetNamedSemaphorePrefix.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/semaphor.llb/GetNamedSemaphorePrefix.vi"/>
				<Item Name="GetRTHostConnectedProp.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/GetRTHostConnectedProp.vi"/>
				<Item Name="Longest Line Length in Pixels.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/Longest Line Length in Pixels.vi"/>
				<Item Name="LVBoundsTypeDef.ctl" Type="VI" URL="/&lt;vilib&gt;/Utility/miscctls.llb/LVBoundsTypeDef.ctl"/>
				<Item Name="NI_AAL_Filter.lvlib" Type="Library" URL="/&lt;vilib&gt;/Analysis/NI_AAL_Filter.lvlib"/>
				<Item Name="NI_AALBase.lvlib" Type="Library" URL="/&lt;vilib&gt;/Analysis/NI_AALBase.lvlib"/>
				<Item Name="NI_AALPro.lvlib" Type="Library" URL="/&lt;vilib&gt;/Analysis/NI_AALPro.lvlib"/>
				<Item Name="NI_DigFilter_Design.lvlib" Type="Library" URL="/&lt;vilib&gt;/addons/Digital Filter Design/NI_DigFilter_Design.lvlib"/>
				<Item Name="NI_DigFilter_FXP.lvlib" Type="Library" URL="/&lt;vilib&gt;/addons/Digital Filter Design/NI_DigFilter_FXP.lvlib"/>
				<Item Name="NI_DigFilter_Process.lvlib" Type="Library" URL="/&lt;vilib&gt;/addons/Digital Filter Design/NI_DigFilter_Process.lvlib"/>
				<Item Name="NI_DigFilterLicensed.lvlib" Type="Library" URL="/&lt;vilib&gt;/addons/Digital Filter Design/NI_DigFilterLicensed.lvlib"/>
				<Item Name="NI_FileType.lvlib" Type="Library" URL="/&lt;vilib&gt;/Utility/lvfile.llb/NI_FileType.lvlib"/>
				<Item Name="NI_Gmath.lvlib" Type="Library" URL="/&lt;vilib&gt;/gmath/NI_Gmath.lvlib"/>
				<Item Name="NI_Matrix.lvlib" Type="Library" URL="/&lt;vilib&gt;/Analysis/Matrix/NI_Matrix.lvlib"/>
				<Item Name="NI_PackedLibraryUtility.lvlib" Type="Library" URL="/&lt;vilib&gt;/Utility/LVLibp/NI_PackedLibraryUtility.lvlib"/>
				<Item Name="Not A Semaphore.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/semaphor.llb/Not A Semaphore.vi"/>
				<Item Name="Not Found Dialog.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/Not Found Dialog.vi"/>
				<Item Name="Number of Waveform Samples.vi" Type="VI" URL="/&lt;vilib&gt;/Waveform/WDTOps.llb/Number of Waveform Samples.vi"/>
				<Item Name="Obtain Semaphore Reference.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/semaphor.llb/Obtain Semaphore Reference.vi"/>
				<Item Name="Release Semaphore Reference.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/semaphor.llb/Release Semaphore Reference.vi"/>
				<Item Name="Release Semaphore.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/semaphor.llb/Release Semaphore.vi"/>
				<Item Name="remez_band specification.ctl" Type="VI" URL="/&lt;vilib&gt;/addons/Digital Filter Design/_REMEZ.llb/remez_band specification.ctl"/>
				<Item Name="remez_Filter Design Info.ctl" Type="VI" URL="/&lt;vilib&gt;/addons/Digital Filter Design/_REMEZ.llb/remez_Filter Design Info.ctl"/>
				<Item Name="remez_Filter Implementation Info.ctl" Type="VI" URL="/&lt;vilib&gt;/addons/Digital Filter Design/_REMEZ.llb/remez_Filter Implementation Info.ctl"/>
				<Item Name="remez_Filter Specification Parsing States.ctl" Type="VI" URL="/&lt;vilib&gt;/addons/Digital Filter Design/_REMEZ.llb/remez_Filter Specification Parsing States.ctl"/>
				<Item Name="remez_fir order and types.ctl" Type="VI" URL="/&lt;vilib&gt;/addons/Digital Filter Design/_REMEZ.llb/remez_fir order and types.ctl"/>
				<Item Name="remez_single band description.ctl" Type="VI" URL="/&lt;vilib&gt;/addons/Digital Filter Design/_REMEZ.llb/remez_single band description.ctl"/>
				<Item Name="RemoveNamedSemaphorePrefix.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/semaphor.llb/RemoveNamedSemaphorePrefix.vi"/>
				<Item Name="Search and Replace Pattern.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/Search and Replace Pattern.vi"/>
				<Item Name="Semaphore RefNum" Type="VI" URL="/&lt;vilib&gt;/Utility/semaphor.llb/Semaphore RefNum"/>
				<Item Name="Semaphore Refnum Core.ctl" Type="VI" URL="/&lt;vilib&gt;/Utility/semaphor.llb/Semaphore Refnum Core.ctl"/>
				<Item Name="Set Bold Text.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/Set Bold Text.vi"/>
				<Item Name="Set String Value.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/Set String Value.vi"/>
				<Item Name="Simple Error Handler.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/Simple Error Handler.vi"/>
				<Item Name="Space Constant.vi" Type="VI" URL="/&lt;vilib&gt;/dlg_ctls.llb/Space Constant.vi"/>
				<Item Name="TagReturnType.ctl" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/TagReturnType.ctl"/>
				<Item Name="Three Button Dialog CORE.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/Three Button Dialog CORE.vi"/>
				<Item Name="Three Button Dialog.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/Three Button Dialog.vi"/>
				<Item Name="Trim Whitespace.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/Trim Whitespace.vi"/>
				<Item Name="Validate Semaphore Size.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/semaphor.llb/Validate Semaphore Size.vi"/>
				<Item Name="Waveform Duration.vi" Type="VI" URL="/&lt;vilib&gt;/Waveform/WDTOps.llb/Waveform Duration.vi"/>
				<Item Name="WDT Get Final Time Value DBL.vi" Type="VI" URL="/&lt;vilib&gt;/Waveform/WDTOps.llb/WDT Get Final Time Value DBL.vi"/>
				<Item Name="WDT Number of Waveform Samples CDB.vi" Type="VI" URL="/&lt;vilib&gt;/Waveform/WDTOps.llb/WDT Number of Waveform Samples CDB.vi"/>
				<Item Name="WDT Number of Waveform Samples DBL.vi" Type="VI" URL="/&lt;vilib&gt;/Waveform/WDTOps.llb/WDT Number of Waveform Samples DBL.vi"/>
				<Item Name="WDT Number of Waveform Samples EXT.vi" Type="VI" URL="/&lt;vilib&gt;/Waveform/WDTOps.llb/WDT Number of Waveform Samples EXT.vi"/>
				<Item Name="WDT Number of Waveform Samples I8.vi" Type="VI" URL="/&lt;vilib&gt;/Waveform/WDTOps.llb/WDT Number of Waveform Samples I8.vi"/>
				<Item Name="WDT Number of Waveform Samples I16.vi" Type="VI" URL="/&lt;vilib&gt;/Waveform/WDTOps.llb/WDT Number of Waveform Samples I16.vi"/>
				<Item Name="WDT Number of Waveform Samples I32.vi" Type="VI" URL="/&lt;vilib&gt;/Waveform/WDTOps.llb/WDT Number of Waveform Samples I32.vi"/>
				<Item Name="WDT Number of Waveform Samples SGL.vi" Type="VI" URL="/&lt;vilib&gt;/Waveform/WDTOps.llb/WDT Number of Waveform Samples SGL.vi"/>
				<Item Name="WDT Waveform Duration DBL.vi" Type="VI" URL="/&lt;vilib&gt;/Waveform/WDTOps.llb/WDT Waveform Duration DBL.vi"/>
				<Item Name="whitespace.ctl" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/whitespace.ctl"/>
			</Item>
			<Item Name="9871 COM In.ctl" Type="VI" URL="../FPGA/9871 COM In.ctl"/>
			<Item Name="Alicat.lvlib" Type="Library" URL="../../../Devices/Alicat/Alicat.lvlib"/>
			<Item Name="Atmospheric Library.lvlib" Type="Library" URL="../../../Reusable/General/Atmospheric/Atmospheric Library.lvlib"/>
			<Item Name="Base Data Class.lvlib" Type="Library" URL="../../../Reusable/Data/Base Data Class.lvlib"/>
			<Item Name="calData.ctl" Type="VI" URL="../../../Reusable/CRDS/Data/Cell/calData.ctl"/>
			<Item Name="crd_data_enum.ctl" Type="VI" URL="../../../Reusable/CRDS/Data/Main/crd_data_enum.ctl"/>
			<Item Name="CRDS Cell Data.lvclass" Type="LVClass" URL="../../../Reusable/CRDS/Data/Cell/CRDS Cell Data.lvclass"/>
			<Item Name="CRDS Data.lvclass" Type="LVClass" URL="../../../Reusable/CRDS/Data/Main/CRDS Data.lvclass"/>
			<Item Name="extparam.ctl" Type="VI" URL="../../../Reusable/CRDS/Data/Cell/extparam.ctl"/>
			<Item Name="File Observer.lvclass" Type="LVClass" URL="../../../Reusable/File IO/Class/File Observer.lvclass"/>
			<Item Name="File Session.lvclass" Type="LVClass" URL="../../../Reusable/Session APIs/File Access/Base/File Session.lvclass"/>
			<Item Name="File Utilities.lvlib" Type="Library" URL="../../../Reusable/File IO/File Utilities.lvlib"/>
			<Item Name="Get Tau Method.ctl" Type="VI" URL="../../../Reusable/CRDS/Cell/Get Tau Method.ctl"/>
			<Item Name="lvanlys.dll" Type="Document" URL="/&lt;resource&gt;/lvanlys.dll"/>
			<Item Name="LVDFD.dll" Type="Document" URL="/&lt;resource&gt;/LVDFD.dll"/>
			<Item Name="Math Utilities.lvlib" Type="Library" URL="../../../Reusable/General/Math Utilities.lvlib"/>
			<Item Name="Mean-SGL.vi" Type="VI" URL="../../../Reusable/General/Math/Mean-SGL.vi"/>
			<Item Name="Nicholas.lvbitx" Type="Document" URL="../FPGA Bitfiles/Nicholas.lvbitx"/>
			<Item Name="Nicholas_FPGATarget_TestRD_yTEjeKL8TuA.lvbitx" Type="Document" URL="../FPGA Bitfiles/Nicholas_FPGATarget_TestRD_yTEjeKL8TuA.lvbitx"/>
			<Item Name="Nicholas_FPGATarget_TestSerial_61+s5lxVKKg.lvbitx" Type="Document" URL="../FPGA Bitfiles/Nicholas_FPGATarget_TestSerial_61+s5lxVKKg.lvbitx"/>
			<Item Name="niFpgaHostInterfaceSession.ctl" Type="VI" URL="../../../../../../../Program Files (x86)/National Instruments/LabVIEW 2013/Targets/NI/FPGA/StockFPGA_IntfPrivate/ScriptTemplates/niFpgaHostInterfaceSession.ctl"/>
			<Item Name="NiFpgaLv.dll" Type="Document" URL="NiFpgaLv.dll">
				<Property Name="NI.PreserveRelativePath" Type="Bool">true</Property>
			</Item>
			<Item Name="niFpgaNodeNameForErrorReporting.ctl" Type="VI" URL="/&lt;vilib&gt;/rvi/interface/common/niFpgaNodeNameForErrorReporting.ctl"/>
			<Item Name="niFpgaWaitOnOcc.vi" Type="VI" URL="/&lt;vilib&gt;/rvi/interface/common/niFpgaWaitOnOcc.vi"/>
			<Item Name="niLvFpga_AcknowledgeIrq_cRIO-9081.vi" Type="VI" URL="/&lt;vilib&gt;/FPGAPlugInAG/cRIO-9081/niLvFpga_AcknowledgeIrq_cRIO-9081.vi"/>
			<Item Name="niLvFpga_Close_cRIO-9081.vi" Type="VI" URL="/&lt;vilib&gt;/FPGAPlugInAG/cRIO-9081/niLvFpga_Close_cRIO-9081.vi"/>
			<Item Name="niLvFpga_ConfigureFifo_cRIO-9081.vi" Type="VI" URL="/&lt;vilib&gt;/FPGAPlugInAG/cRIO-9081/niLvFpga_ConfigureFifo_cRIO-9081.vi"/>
			<Item Name="niLvFpga_Open_cRIO-9081.vi" Type="VI" URL="/&lt;vilib&gt;/FPGAPlugInAG/cRIO-9081/niLvFpga_Open_cRIO-9081.vi"/>
			<Item Name="niLvFpga_ReadFifo_cRIO-9081.vi" Type="VI" URL="/&lt;vilib&gt;/FPGAPlugInAG/cRIO-9081/niLvFpga_ReadFifo_cRIO-9081.vi"/>
			<Item Name="niLvFpga_StartFifo_cRIO-9081.vi" Type="VI" URL="/&lt;vilib&gt;/FPGAPlugInAG/cRIO-9081/niLvFpga_StartFifo_cRIO-9081.vi"/>
			<Item Name="niLvFpga_WaitOnIrq_cRIO-9081.vi" Type="VI" URL="/&lt;vilib&gt;/FPGAPlugInAG/cRIO-9081/niLvFpga_WaitOnIrq_cRIO-9081.vi"/>
			<Item Name="niLvFpgaAdjustHostInterfaceError.vi" Type="VI" URL="/&lt;vilib&gt;/rvi/errors/niLvFpgaAdjustHostInterfaceError.vi"/>
			<Item Name="niLvFpgaErrorClusterFromErrorCode.vi" Type="VI" URL="/&lt;vilib&gt;/rvi/errors/niLvFpgaErrorClusterFromErrorCode.vi"/>
			<Item Name="niLvFpgaFormatErrorSource.vi" Type="VI" URL="/&lt;vilib&gt;/rvi/errors/niLvFpgaFormatErrorSource.vi"/>
			<Item Name="niLvFpgaMergeErrorWithErrorCode.vi" Type="VI" URL="/&lt;vilib&gt;/rvi/errors/niLvFpgaMergeErrorWithErrorCode.vi"/>
			<Item Name="niLvFpgaWhatHappensToTopLevelVI.ctl" Type="VI" URL="/&lt;vilib&gt;/rvi/errors/niLvFpgaWhatHappensToTopLevelVI.ctl"/>
			<Item Name="nirio_resource_hc.ctl" Type="VI" URL="/&lt;vilib&gt;/userdefined/High Color/nirio_resource_hc.ctl"/>
			<Item Name="nirviErrorClusterFromErrorCode.vi" Type="VI" URL="/&lt;vilib&gt;/RVI Host/nirviSupport.llb/nirviErrorClusterFromErrorCode.vi"/>
			<Item Name="nirviWhatTheDeviceIsDoing.ctl" Type="VI" URL="/&lt;vilib&gt;/rvi/ClientSDK/nirviWhatTheDeviceIsDoing.ctl"/>
			<Item Name="Observer.lvclass" Type="LVClass" URL="../../../Reusable/Observer/Observer.lvclass"/>
			<Item Name="Parse Model String.vi" Type="VI" URL="../../../Devices/Alicat/Parse Model String.vi"/>
			<Item Name="Port Selector.ctl" Type="VI" URL="../FPGA/Port Selector.ctl"/>
			<Item Name="Serial IO.lvclass" Type="LVClass" URL="../../../Reusable/Serial IO/Serial IO.lvclass"/>
			<Item Name="StockFPGA_InlinedAdjustHostInterfaceError.vi" Type="VI" URL="../../../../../../../Program Files (x86)/National Instruments/LabVIEW 2013/Targets/NI/FPGA/StockFPGA_IntfPrivate/ScriptTemplates/StockFPGA_InlinedAdjustHostInterfaceError.vi"/>
			<Item Name="StockFPGA_InlinedMethodWaitForSingleIRQ.vi" Type="VI" URL="/&lt;vilib&gt;/rvi/interface/nirviMethod/StockFPGA_InlinedMethodWaitForSingleIRQ.vi"/>
			<Item Name="StockFPGA_InlinedWaitFor1OrMoreIRQs.vi" Type="VI" URL="../../../../../../../Program Files (x86)/National Instruments/LabVIEW 2013/Targets/NI/FPGA/StockFPGA_IntfPrivate/ScriptTemplates/StockFPGA_InlinedWaitFor1OrMoreIRQs.vi"/>
			<Item Name="StockFPGA_PlugInOptionalWaitOnIrq.vi" Type="VI" URL="../../../../../../../Program Files (x86)/National Instruments/LabVIEW 2013/Targets/NI/FPGA/StockFPGA_IntfPrivate/ScriptTemplates/StockFPGA_PlugInOptionalWaitOnIrq.vi"/>
			<Item Name="String Data.lvclass" Type="LVClass" URL="../../../Reusable/Data/String/String Data.lvclass"/>
			<Item Name="System Log.lvclass" Type="LVClass" URL="../../../Reusable/Session Log/System Log.lvclass"/>
			<Item Name="XDNodeRunTimeDep.lvlib" Type="Library" URL="/&lt;vilib&gt;/Platform/TimedLoop/XDataNode/XDNodeRunTimeDep.lvlib"/>
		</Item>
		<Item Name="Build Specifications" Type="Build">
			<Item Name="Tims App" Type="{69A947D5-514E-4E75-818E-69657C0547D8}">
				<Property Name="App_copyErrors" Type="Bool">true</Property>
				<Property Name="App_INI_aliasGUID" Type="Str">{F11A8470-ABEF-4D7C-9D19-43756F15886E}</Property>
				<Property Name="App_INI_GUID" Type="Str">{A7C11DB0-4F00-4787-943E-6D11F4317467}</Property>
				<Property Name="App_serverConfig.httpPort" Type="Int">8002</Property>
				<Property Name="App_winsec.description" Type="Str">http://www.NOAA.com</Property>
				<Property Name="Bld_autoIncrement" Type="Bool">true</Property>
				<Property Name="Bld_buildCacheID" Type="Str">{694B337A-29DD-4C5B-8C9F-82AC0C94BE26}</Property>
				<Property Name="Bld_buildSpecName" Type="Str">Tims App</Property>
				<Property Name="Bld_excludeInlineSubVIs" Type="Bool">true</Property>
				<Property Name="Bld_excludeLibraryItems" Type="Bool">true</Property>
				<Property Name="Bld_excludePolymorphicVIs" Type="Bool">true</Property>
				<Property Name="Bld_localDestDir" Type="Path">../builds/NI_AB_PROJECTNAME/NI_AB_TARGETNAME/Tims App</Property>
				<Property Name="Bld_localDestDirType" Type="Str">relativeToCommon</Property>
				<Property Name="Bld_modifyLibraryFile" Type="Bool">true</Property>
				<Property Name="Bld_previewCacheID" Type="Str">{832E462E-AAD0-4B21-8A5B-74795F145FD7}</Property>
				<Property Name="Bld_targetDestDir" Type="Path">/c/ni-rt/startup</Property>
				<Property Name="Bld_version.build" Type="Int">10</Property>
				<Property Name="Bld_version.major" Type="Int">1</Property>
				<Property Name="Destination[0].destName" Type="Str">startup.rtexe</Property>
				<Property Name="Destination[0].path" Type="Path">/c/ni-rt/startup/startup.rtexe</Property>
				<Property Name="Destination[0].path.type" Type="Str">&lt;none&gt;</Property>
				<Property Name="Destination[0].preserveHierarchy" Type="Bool">true</Property>
				<Property Name="Destination[0].type" Type="Str">App</Property>
				<Property Name="Destination[1].destName" Type="Str">Support Directory</Property>
				<Property Name="Destination[1].path" Type="Path">/c/ni-rt/startup/data</Property>
				<Property Name="Destination[1].path.type" Type="Str">&lt;none&gt;</Property>
				<Property Name="DestinationCount" Type="Int">2</Property>
				<Property Name="Source[0].itemID" Type="Str">{A1E81653-F0F1-402A-9A55-17FF331B7D04}</Property>
				<Property Name="Source[0].type" Type="Str">Container</Property>
				<Property Name="Source[1].destinationIndex" Type="Int">0</Property>
				<Property Name="Source[1].itemID" Type="Ref">/RT CompactRIO Target/Tim Server.vi</Property>
				<Property Name="Source[1].sourceInclusion" Type="Str">TopLevel</Property>
				<Property Name="Source[1].type" Type="Str">VI</Property>
				<Property Name="Source[2].destinationIndex" Type="Int">0</Property>
				<Property Name="Source[2].itemID" Type="Ref">/RT CompactRIO Target/Nick SV.lvlib</Property>
				<Property Name="Source[2].Library.allowMissingMembers" Type="Bool">true</Property>
				<Property Name="Source[2].sourceInclusion" Type="Str">Include</Property>
				<Property Name="Source[2].type" Type="Str">Library</Property>
				<Property Name="SourceCount" Type="Int">3</Property>
				<Property Name="TgtF_companyName" Type="Str">NOAA</Property>
				<Property Name="TgtF_fileDescription" Type="Str">Tims App</Property>
				<Property Name="TgtF_internalName" Type="Str">Tims App</Property>
				<Property Name="TgtF_legalCopyright" Type="Str">Copyright © 2013 NOAA</Property>
				<Property Name="TgtF_productName" Type="Str">Tims App</Property>
				<Property Name="TgtF_targetfileGUID" Type="Str">{E74E8E57-22DE-49BB-8ACE-FD926A4C4AE4}</Property>
				<Property Name="TgtF_targetfileName" Type="Str">startup.rtexe</Property>
			</Item>
		</Item>
	</Item>
</Project>
