Imports System.Collections.Concurrent
Imports System.IO
Imports System.Text
Imports System.Data.SQLite
Imports System.Threading
Imports System.Timers
Imports Timer = System.Timers.Timer

Module Program
    Public userCoordinates As (Integer, Integer, Integer)
    Dim objectDictionary As New ConcurrentDictionary(Of Integer, MyObject)
    Dim uniqId As Integer = 0
    Dim objectsIntersectionDict As New ConcurrentDictionary(Of Integer, Tuple(Of (X As Double, Y As Double, Z As Double), (X As Double, Y As Double, Z As Double)))
    ' *                                                                 ^^ objID whiteCoordinates(current objID coords) blackCoordinates(past objID coords)

    Public panelsArray() As (PanelType As PanelType, FirstTuple As (Integer, Integer, Integer), SecondTuple As (Integer, Integer, Integer))
    'Public panelNormalsArray() As (PanelName As String, Normal As (Integer, Integer, Integer), SecondTuple As (Integer, Integer, Integer))
    Public panelNormalsArray() As (PanelType As PanelType, Normal As (Integer, Integer, Integer), SecondTuple As (Integer, Integer, Integer))
    Public panelName_CornerArray() As (PanelName_Corner As String, Corner As (Integer, Integer, Integer))
    Dim panelData As New PanelDataManager


    Public Class Program
        Public Shared Sub Main()

            ' =Load 3D objects=
            'CreateAndAddPregeneratedObjects()
            CreateUCSIcon()
            'CreateBarExtensions()
            Console.WriteLine("3D objects loaded")


            Dim produceWorker As New ComponentModel.BackgroundWorker
            Dim doneEvent As New AutoResetEvent(False)
            produceWorker.WorkerSupportsCancellation = True
            AddHandler produceWorker.DoWork,
                Sub(sender As Object, e As ComponentModel.DoWorkEventArgs)
                    While Not produceWorker.CancellationPending ' Infinite loop
                        ConsumeData() 'game loop
                    End While
                End Sub
            AddHandler produceWorker.RunWorkerCompleted,
                Sub(sender As Object, e As ComponentModel.RunWorkerCompletedEventArgs)
                    If e.Error IsNot Nothing Then
                        ' Handle the error
                        Console.WriteLine("e.Error")
                    ElseIf e.Cancelled Then
                        ' Handle cancellation
                        Console.WriteLine("e.Cancelled")
                    Else
                        ' Handle completion
                        Console.WriteLine("completion")
                    End If
                    doneEvent.Set() ' Signal doneEvent to unblock the main thread
                End Sub
            produceWorker.RunWorkerAsync()
            doneEvent.WaitOne() ' Main thread waits here until RunWorkerCompleted event signals
        End Sub

        Private Shared Sub ProduceData()
            While True 'Runs indefinitely
                For Each objectId In objectDictionary.Keys
                    Dim obj As MyObject = Nothing
                    If objectDictionary.TryGetValue(objectId, obj) Then
                        ' Just read the object's location without updating it
                        Dim currentLocation As Coordinates3D = obj.Location
                        ' ok you could writeline this.
                        ' * ProduceData nolonger does anything *
                        ' we can use it to effect the objects on another thread where our apps run?
                    End If
                Next
                Thread.Sleep(50)
            End While
        End Sub

        Public Shared Sub ConsumeData()

            'Encoding.RegisterProvider(CodePagesEncodingProvider.Instance)
            'Dim enc1252 = Encoding.GetEncoding(1252)
            'CreateBlockUpdatesDatabaseStructure()

            '=  apps  =
            'Dim DemoApplication As New DemoApp
            'Dim RWords As New WordGeneratorApp
            'Dim FrameCounter As New FrameCounterApp    'we will be using one of these demo apps for the 3d programs.

            '=end apps=

            Dim i As Integer = 0

            Dim doIGo As New Boolean
            doIGo = False

            While i < 1
                i += 1
                i -= 1

                Dim minDistanceCoordinates As New List(Of (Integer, Integer, Integer)) ' clear me when needed** *

                Dim flag As String
                Dim playerdata As String
                Dim newflagsvalue As List(Of String)
                newflagsvalue = tryToGetTheValue()
                flag = newflagsvalue(0)
                playerdata = newflagsvalue(1)

                Dim possibleCoordINDEXvalue As String = tryToGetTheCOORDINDEXValue().ToString ' ]*[ that's why it has to be one of the correct values to start. in the sqlite file. ]*[

                If flag = possibleCoordINDEXvalue Then
                    'do nothing
                Else

                    Select Case possibleCoordINDEXvalue '(a pre-assigned integer variable by you).
                        Case "01" To "11"
                            doIGo = True
                        Case "bb"
                            doIGo = True
                        Case Else
                            doIGo = False
                            'do nothing
                    End Select

                    Dim stopwatch As Stopwatch = Stopwatch.StartNew()

                    If doIGo = True Then

                        fillUserCoordinates(playerdata)
                        'Console.WriteLine(objectDictionary.Count.ToString)

                        Dim bounds As New PanelBounds()
                        Parallel.ForEach(objectDictionary.Keys, Sub(objectID)

                                                                    Dim obj As MyObject = Nothing
                                                                    If objectDictionary.TryGetValue(objectID, obj) Then

                                                                        ' Your processing logic here
                                                                        Dim x As Long = obj.Location.X
                                                                        Dim y As Long = obj.Location.Y
                                                                        Dim z As Long = obj.Location.Z

                                                                        Dim rayPnt As (Integer, Integer, Integer) = (x, y, z)

                                                                        Dim intersections As New Dictionary(Of PanelType, Vector3D)

                                                                        For Each panel In panelNormalsArray
                                                                            Dim intersect = GetIntersection(rayPnt, userCoordinates, panel.Item2, panel.Item3)
                                                                            ' Console.WriteLine("Intersection for {0}: {1}, Intersection exists: {2}", panel.Item1, intersect.Item1, intersect.Item2)

                                                                            If intersect.Item2 Then
                                                                                intersections.Add(panel.PanelType, intersect.Item1)
                                                                            End If
                                                                        Next

                                                                        For Each panelName In intersections.Keys
                                                                            Dim panelIntersection = intersections(panelName)
                                                                            ' Console.WriteLine("{0} intersection point: {1}", panelName, panelIntersection)
                                                                        Next

                                                                        Dim intersectionsWithinBounds As New Dictionary(Of PanelType, Vector3D)()

                                                                        For Each panelType In intersections.Keys
                                                                            Dim panelIntersection = intersections(panelType)
                                                                            Dim coords As String() = panelIntersection.ToString().Split(", ")

                                                                            Try
                                                                                Dim point As (Integer, Integer, Integer) = (CInt(coords(0)), CInt(coords(1)), CInt(coords(2)))
                                                                                Dim isWithinBounds As Boolean = bounds.IsPointWithinPanel(panelType, point)
                                                                                ' Console.WriteLine($"{panelName} intersection point {panelIntersection} is within bounds: {isWithinBounds}")

                                                                                If isWithinBounds Then
                                                                                    intersectionsWithinBounds.Add(panelType, panelIntersection)
                                                                                End If
                                                                            Catch e As OverflowException ' Handle the exception
                                                                                'Console.WriteLine("One of the coordinates is too large or too small to fit into an integer.")
                                                                                'Do nothing
                                                                            End Try

                                                                        Next

                                                                        For Each kvp In intersectionsWithinBounds
                                                                            ' Console.WriteLine($"Panel: {kvp.Key}, Intersection: {kvp.Value}")
                                                                        Next

                                                                        Dim points As Dictionary(Of PanelType, (Integer, Integer, Integer)) = intersectionsWithinBounds.ToDictionary(Function(pair) pair.Key, Function(pair) pair.Value.ToIntTuple())
                                                                        Dim distances As Dictionary(Of PanelType, Single) = points.ToDictionary(Function(pair) pair.Key, Function(pair) CalculateFastDistance(rayPnt, pair.Value))
                                                                        Dim minDistance As Single = Single.MaxValue
                                                                        Dim minDistanceCoordinate As (Integer, Integer, Integer) = (0, 0, 0)

                                                                        For Each distancePair In distances
                                                                            If distancePair.Value < minDistance Then
                                                                                minDistance = distancePair.Value
                                                                                minDistanceCoordinate = points(distancePair.Key)
                                                                            End If
                                                                        Next

                                                                        'Dim minDistanceString As String = $"{minDistanceCoordinate.Item1},{minDistanceCoordinate.Item2},{minDistanceCoordinate.Item3};"
                                                                        'Dim whiteCoordinatesBuilder As New StringBuilder(minDistanceString)
                                                                        'Dim blackCoordinatesBuilder As New StringBuilder("-370,73,-200;")

                                                                        Dim individualObject As (ObjID As Integer, X As Double, Y As Double, Z As Double) = (obj.UniqIdentifier, minDistanceCoordinate.Item1, minDistanceCoordinate.Item2, minDistanceCoordinate.Item3)

                                                                        If Not objectsIntersectionDict.ContainsKey(individualObject.ObjID) Then

                                                                            Dim added As Boolean = objectsIntersectionDict.TryAdd(individualObject.ObjID, New Tuple(Of (Double, Double, Double), (Double, Double, Double))((individualObject.X, individualObject.Y, individualObject.Z), (0, 0, 0)))

                                                                            'objectsIntersectionDict.Add(individualObject.ObjID, (New Tuple(Of (Double, Double, Double), (Double, Double, Double))((individualObject.X, individualObject.Y, individualObject.Z), (0, 0, 0))))
                                                                        Else
                                                                            Dim existingValues = objectsIntersectionDict(individualObject.ObjID).Item1 ' * 
                                                                            If existingValues.X <> individualObject.X Or existingValues.Y <> individualObject.Y Or existingValues.Z <> individualObject.Z Then
                                                                                Dim previousValue = existingValues ' *
                                                                                objectsIntersectionDict(individualObject.ObjID) = New Tuple(Of (Double, Double, Double), (Double, Double, Double))((individualObject.X, individualObject.Y, individualObject.Z), previousValue)
                                                                            End If
                                                                        End If


                                                                    End If

                                                                End Sub)

                        Dim uniqueCurrentCoordinatesSet As New HashSet(Of String) ' HashSets to store unique current and previous coordinates explicitly.
                        Dim uniquePreviousCoordinatesSet As New HashSet(Of String)

                        For Each item As KeyValuePair(Of Integer, Tuple(Of (X As Double, Y As Double, Z As Double), (X As Double, Y As Double, Z As Double)))
                                    In objectsIntersectionDict
                            ' Add the current coordinates as strings in the HashSet to ensure uniqueness.
                            uniqueCurrentCoordinatesSet.Add($"{item.Value.Item1.X},{item.Value.Item1.Y},{item.Value.Item1.Z}")

                            ' *      previous 
                            uniquePreviousCoordinatesSet.Add($"{item.Value.Item2.X},{item.Value.Item2.Y},{item.Value.Item2.Z}")

                        Next

                        ' == difference engine work here ==   


                        Dim currentCoordinatesSb As New StringBuilder()
                        Dim previousCoordinatesSb As New StringBuilder()

                        For Each coordinate As String In uniqueCurrentCoordinatesSet ' * WHITE
                            If currentCoordinatesSb.Length > 0 Then
                                currentCoordinatesSb.Append(";")
                            End If

                            currentCoordinatesSb.Append(coordinate)

                        Next

                        For Each coordinate As String In uniquePreviousCoordinatesSet ' * BLACK
                            If previousCoordinatesSb.Length > 0 Then
                                previousCoordinatesSb.Append(";")
                            End If

                            previousCoordinatesSb.Append(coordinate)

                        Next

                        Dim coordinatesTuple2 As Tuple(Of StringBuilder, StringBuilder) = New Tuple(Of StringBuilder, StringBuilder)(currentCoordinatesSb, previousCoordinatesSb)






                        allFrameTuples.Add(coordinatesTuple2)






                        'delete coordinates
                        Dim PathAndNameOfBlockUpdatesSQLite As String = String.Concat(IO.Path.Combine(IO.Path.GetFullPath(Directory.GetCurrentDirectory), "RamDiskLocation"), "/BlockUpdates.sqlite")
                        Using myConnection As SQLiteConnection = New SQLiteConnection()
                            Dim myConnectionString As String
                            myConnectionString = "Data Source=" + PathAndNameOfBlockUpdatesSQLite + ";Version=3; Journal Mode = OFF;"
                            myConnection.ConnectionString = myConnectionString
                            myConnection.Open()
                            Using tx = myConnection.BeginTransaction()
                                Using cmd = myConnection.CreateCommand()
                                    Try
                                        Dim myColumnData = "DELETE FROM coords "
                                        cmd.CommandText = myColumnData
                                        cmd.ExecuteNonQuery()
                                    Catch ex As Exception
                                        Console.WriteLine(ex.ToString)
                                    End Try
                                End Using
                                tx.Commit()
                            End Using
                        End Using

                        InsertSQLiteValues("coords", testBlocksPlusBlackAndWhite(newflagsvalue, totalFrameTuple))

                        allFrameTuples.Clear()

                        '#==update apps below==#

                        'RWords.submitSpatialZones() 'would be nice if, could tell all apps to submit sz'z.
                        'FrameCounter.submitSpatialZones()

                        '#====================#
                    Else
                    End If







                    Dim ticks As Long = stopwatch.ElapsedTicks
                    Dim nanosecondsPerTick As Double = (1000000000.0 / Stopwatch.Frequency)
                    Dim elapsedNanoseconds As Double = ticks * nanosecondsPerTick
                    Dim elapsedMilliseconds As Double = elapsedNanoseconds / 1000000.0
                    'Console.WriteLine(elapsedMilliseconds)








                End If

            End While
        End Sub

    End Class



    ' ============== MOVE TO 3D APP ================
    Sub CreateUCSIcon()
        Dim i As Integer
        For i = 0 To 5000 Step 1
            AddMyObjectToFactory(i, 0, 0) ' Points on X-axis
            AddMyObjectToFactory(0, i, 0) ' Points on Y-axis
            AddMyObjectToFactory(0, 0, i) ' Points on Z-axis
        Next i
    End Sub
    Sub CreateBarExtensions()
        AddMyObjectToFactory(-129, 160, -69)
        AddMyObjectToFactory(-128, 160, -69)
        AddMyObjectToFactory(-127, 160, -69)
        AddMyObjectToFactory(-126, 160, -69)
        AddMyObjectToFactory(-125, 160, -69)
        Dim i As Integer
        For i = -124 To -50
            AddMyObjectToFactory(i, 160, -69)
        Next
        For i = 161 To 165
            AddMyObjectToFactory(-50, i, -69)
        Next
        For i = 159 To 155 Step -1
            AddMyObjectToFactory(-50, i, -69)
        Next
        AddMyObjectToFactory(-371, 160, -69)
        AddMyObjectToFactory(-372, 160, -69)
        AddMyObjectToFactory(-373, 160, -69)
        AddMyObjectToFactory(-374, 160, -69)
        AddMyObjectToFactory(-375, 160, -69)
        For i = -376 To -450 Step -1
            AddMyObjectToFactory(i, 160, -69)
        Next
    End Sub
    Sub CreateAndAddPregeneratedObjects()
        AddMyObjectToFactory(0, 0, 0) ' zero point of all UCS axes
#Region "one_coord_per_panel"
        'AddMyObjectToFactory(-258, 147, 339)
        'AddMyObjectToFactory(-664, 147, -78)
        'AddMyObjectToFactory(-255, 147, -493)
        'AddMyObjectToFactory(168, 147, -74)
        'AddMyObjectToFactory(-250, 300, -78)
        'AddMyObjectToFactory(-250, -75, -78)
#End Region
    End Sub
    Sub AddMyObjectToFactory(x As Integer, y As Integer, z As Integer)
        Dim key As Integer = GetNextUniqId()
        Dim location As New Coordinates3D(x, y, z)
        Dim obj As New MyObject(location, key)
        objectDictionary.TryAdd(key, obj)
    End Sub
    Function GetNextUniqId() As Integer
        uniqId += 1
        Return uniqId
    End Function
    ' ================ END OF MOVE TO 3D APP ===================

    Public Class Coordinates3D
        Public Property X As Long
        Public Property Y As Long
        Public Property Z As Long

        Public Sub New(x As Long, y As Long, z As Long)
            Me.X = x
            Me.Y = y
            Me.Z = z
        End Sub
    End Class

    Public Class MyObject
        Public Property UniqIdentifier As Integer
        Public Property Location As Coordinates3D
        Private locationUpdateTimer As Timer

        Public Sub New(location As Coordinates3D, key As Integer)
            Me.Location = location
            Me.UniqIdentifier = key
            InitializeLocationUpdateTimer()
        End Sub

        Private Sub InitializeLocationUpdateTimer()
            locationUpdateTimer = New Timer(1000)
            AddHandler locationUpdateTimer.Elapsed, AddressOf UpdateLocation
            locationUpdateTimer.AutoReset = True
            locationUpdateTimer.Enabled = True
        End Sub

        Private Sub UpdateLocation(sender As Object, e As ElapsedEventArgs)
            ' Simulate location change
            Dim newLocation As New Coordinates3D(Me.Location.X, Me.Location.Y, Me.Location.Z)
            Me.Location = newLocation
        End Sub
    End Class


#Region "meat"
    Function GetIntersection(rayPnt As (Long, Long, Long), userCoordinates As (Long, Long, Long),
                         planeNormal As (Long, Long, Long), planePoint As (Long, Long, Long)) As (Vector3D, Boolean)
        Dim rp = New Vector3D(rayPnt.Item1, rayPnt.Item2, rayPnt.Item3)
        Dim observer = New Vector3D(userCoordinates.Item1, userCoordinates.Item2, userCoordinates.Item3)
        Dim pn = New Vector3D(planeNormal.Item1, planeNormal.Item2, planeNormal.Item3)
        Dim pp = New Vector3D(planePoint.Item1, planePoint.Item2, planePoint.Item3)

        Dim rv = rp - observer

        Dim ip = IntersectPoint(rv, rp, pn, pp)

        Dim intersectionExists = (ip IsNot Nothing)

        Return (ip, intersectionExists)

    End Function

    Function CalculateFastDistance(point1 As (Integer, Integer, Integer), point2 As (Integer, Integer, Integer)) As Single
        Dim xDiff As Integer = point2.Item1 - point1.Item1
        Dim yDiff As Integer = point2.Item2 - point1.Item2
        Dim zDiff As Integer = point2.Item3 - point1.Item3

        Return MathF.Sqrt(xDiff * xDiff + yDiff * yDiff + zDiff * zDiff)
    End Function

    Function IntersectPoint(rayVector As Vector3D, rayPoint As Vector3D, planeNormal As Vector3D, planePoint As Vector3D) As Vector3D
        Dim diff = rayPoint - planePoint
        Dim prod1 = diff.Dot(planeNormal)
        Dim prod2 = rayVector.Dot(planeNormal)
        Dim prod3 = prod1 / prod2
        Return rayPoint - rayVector * prod3
    End Function















    Public Class PanelBounds
        Private ReadOnly _precalculatedBounds As Dictionary(Of PanelType, (Integer, Integer, Integer, Integer, Integer, Integer))

        Public Sub New()
            _precalculatedBounds = panelsArray.ToDictionary(Function(panel) panel.PanelType, Function(panel) CalculateMinMaxBounds(panel.Item2, panel.Item3))
        End Sub

        Private Function CalculateMinMaxBounds(corner1 As (Integer, Integer, Integer), corner2 As (Integer, Integer, Integer)) As (Integer, Integer, Integer, Integer, Integer, Integer)
            Dim minX As Integer = Math.Min(corner1.Item1, corner2.Item1)
            Dim maxX As Integer = Math.Max(corner1.Item1, corner2.Item1)
            Dim minY As Integer = Math.Min(corner1.Item2, corner2.Item2)
            Dim maxY As Integer = Math.Max(corner1.Item2, corner2.Item2)
            Dim minZ As Integer = Math.Min(corner1.Item3, corner2.Item3)
            Dim maxZ As Integer = Math.Max(corner1.Item3, corner2.Item3)

            Return (minX, maxX, minY, maxY, minZ, maxZ)
        End Function

        Public Function IsPointWithinPanel(panelType As PanelType, point As (Integer, Integer, Integer)) As Boolean
            If _precalculatedBounds.ContainsKey(panelType) Then
                Dim bounds = _precalculatedBounds(panelType)
                Return (point.Item1 >= bounds.Item1) AndAlso (point.Item1 <= bounds.Item2) AndAlso (point.Item2 >= bounds.Item3) AndAlso (point.Item2 <= bounds.Item4) AndAlso (point.Item3 >= bounds.Item5) AndAlso (point.Item3 <= bounds.Item6)
            Else
                Throw New ArgumentException($"Invalid panel type: {panelType}") ' trim this, For Kirk's sake.
            End If
        End Function

    End Class

    Class Vector3D
        Private ReadOnly x As Double
        Private ReadOnly y As Double
        Private ReadOnly z As Double

        Sub New(nx As Double, ny As Double, nz As Double)
            x = nx
            y = ny
            z = nz
        End Sub

        Public Function Dot(rhs As Vector3D) As Double
            Return x * rhs.x + y * rhs.y + z * rhs.z
        End Function

        Public Shared Operator +(ByVal a As Vector3D, ByVal b As Vector3D) As Vector3D
            Return New Vector3D(a.x + b.x, a.y + b.y, a.z + b.z)
        End Operator

        Public Shared Operator -(ByVal a As Vector3D, ByVal b As Vector3D) As Vector3D
            Return New Vector3D(a.x - b.x, a.y - b.y, a.z - b.z)
        End Operator

        Public Shared Operator *(ByVal a As Vector3D, ByVal b As Double) As Vector3D
            Return New Vector3D(a.x * b, a.y * b, a.z * b)
        End Operator

        Public Overrides Function ToString() As String
            Return String.Format("{0:F}, {1:F}, {2:F}", x, y, z)
        End Function

        Public Function ToIntTuple() As (Integer, Integer, Integer)
            Return (CInt(Fix(x)), CInt(Fix(y)), CInt(Fix(z)))
        End Function
    End Class
#End Region

#Region "Sqlite"
    Function tryToGetTheValue()

        Dim places As New List(Of String)
yolo:
        Dim indexTest As String = RetrieveCHFlag()
        Dim playerData As String = indexTest
        playerData = playerData.Remove(0, 2)

        Dim startIndex As Integer = 0
        Dim length As Integer = 2
        Dim substring As String = indexTest.Substring(startIndex, length)
        Select Case substring
            Case "01"
                places.Add("01")
                places.Add(playerData.ToString)
                Return places
            Case "02"
                places.Add("02")
                places.Add(playerData.ToString)
                Return places
            Case "03"
                places.Add("03")
                places.Add(playerData.ToString)
                Return places
            Case "04"
                places.Add("04")
                places.Add(playerData.ToString)
                Return places
            Case "05"
                places.Add("05")
                places.Add(playerData.ToString)
                Return places
            Case "06"
                places.Add("06")
                places.Add(playerData.ToString)
                Return places
            Case "07"
                places.Add("07")
                places.Add(playerData.ToString)
                Return places
            Case "08"
                places.Add("08")
                places.Add(playerData.ToString)
                Return places
            Case "09"
                places.Add("09")
                places.Add(playerData.ToString)
                Return places
            Case "10"
                places.Add("10")
                places.Add(playerData.ToString)
                Return places
            Case "aa"
                places.Add("aa")
                places.Add(playerData.ToString)
                Return places
            Case "nope"
                places.Add("bb")
                places.Add(playerData.ToString)
                Return places
            Case Else   'Today is: 12/11/2012 12:00:00 AM
                Console.WriteLine("YOLO")
                Console.WriteLine("Current Time: ")
                Dim dt As Date = Now
                Console.WriteLine("Time is: {0}", dt)
                GoTo yolo
        End Select

    End Function


    Function RetrieveCHFlag() As String
        Dim result As String = "nope"
        Dim PathAndNameOfBlockUpdatesSQLite As String = Path.Combine(Directory.GetCurrentDirectory(), "RamDiskLocation", "BlockUpdates.sqlite")
        'Console.WriteLine(PathAndNameOfBlockUpdatesSQLite)
        Try
            Using sqlite_conn As New SQLiteConnection($"Data Source={PathAndNameOfBlockUpdatesSQLite};Version=3;New=True;")
                sqlite_conn.Open()

                Using sqlite_cmd As New SQLiteCommand("SELECT * FROM Did_CH_UpdateMe", sqlite_conn)
                    Using sqlite_datareader As SQLiteDataReader = sqlite_cmd.ExecuteReader()

                        While (sqlite_datareader.Read())
                            Dim textReader As String = sqlite_datareader.GetString(0)
                            result = textReader.ToString
                            ' Console.WriteLine(result) '    ***
                        End While

                    End Using
                End Using
            End Using

        Catch ex As Exception
            Console.WriteLine(ex.Message)
            'Console.ReadLine() ' *****
        End Try

        Return result
    End Function



    Function RetrieveCoord_INDEX_Value() As String
        Dim result As String = "nope"
        Dim PathAndNameOfBlockUpdatesSQLite As String = Path.Combine(Directory.GetCurrentDirectory(), "RamDiskLocation", "BlockUpdates.sqlite")

        Try
            Using sqlite_conn As New SQLiteConnection($"Data Source={PathAndNameOfBlockUpdatesSQLite};Version=3;New=True;")
                sqlite_conn.Open()

                Using sqlite_cmd As New SQLiteCommand("SELECT * FROM coords", sqlite_conn)
                    Using sqlite_datareader As SQLiteDataReader = sqlite_cmd.ExecuteReader()

                        While (sqlite_datareader.Read())
                            Dim textReader As String = sqlite_datareader.GetString(0)
                            result = textReader.ToString
                        End While

                    End Using
                End Using
            End Using

        Catch ex As Exception
            Console.WriteLine(ex.Message)
        End Try

        Return result.Substring(5, 2)
    End Function

    Sub InsertSQLiteValues(Table As String, Value As String)
        Dim PathAndNameOfBlockUpdatesSQLite As String = IO.Path.Combine(IO.Path.GetFullPath(Directory.GetCurrentDirectory), "RamDiskLocation", "BlockUpdates.sqlite")

        If System.IO.File.Exists(PathAndNameOfBlockUpdatesSQLite) Then
            CreateSQLiteData(PathAndNameOfBlockUpdatesSQLite, Table, Value)
        End If

    End Sub



    Sub CreateSQLiteData(sqliteDB As String, tableName As String, dataValue As String)
        Dim myConnectionString As String = String.Format("Data Source={0};Version=3; Journal Mode = OFF;", sqliteDB)

        Using myConnection As SQLiteConnection = New SQLiteConnection(myConnectionString)
            myConnection.Open()
            Using tx = myConnection.BeginTransaction()
                Using cmd = myConnection.CreateCommand()
                    Dim myColumnData = $"INSERT INTO {tableName} VALUES (?);"
                    cmd.CommandText = myColumnData
                    cmd.Parameters.AddWithValue("?", dataValue)
                    Try
                        cmd.ExecuteNonQuery()
                    Catch ex As Exception
                        Console.WriteLine("An error occurred: " & ex.ToString)
                    End Try
                End Using
                tx.Commit()
            End Using
        End Using
    End Sub



    Sub CreateBlockUpdatesDatabaseStructure()
        Dim PathAndNameOfBlockUpdatesSQLite As String = String.Concat(IO.Path.Combine(IO.Path.GetFullPath(Directory.GetCurrentDirectory), "RamDiskLocation"), "/BlockUpdates.sqlite")

        If System.IO.File.Exists(PathAndNameOfBlockUpdatesSQLite) Then
            Console.WriteLine(" Old Database Found: " + PathAndNameOfBlockUpdatesSQLite + " Please reset.")
        Else
            Console.WriteLine("Not Found: " + PathAndNameOfBlockUpdatesSQLite)
            SQLiteConnection.CreateFile(PathAndNameOfBlockUpdatesSQLite)

            Dim BlockUpdates_coords_Tablename As String = "coords"
            Dim BlockUpdatesColumnName_forcoordstable As String = "New_String"
            Dim BlockUpdates_Did_VB_UpdateMe_Tablename As String = "Did_VB_UpdateMe"
            Dim BlockUpdatesColumnName_forDid_VB_UpdateMetable As String = "VB_Flag"
            Dim BlockUpdates_Did_CH_UpdateMe_Tablename As String = "Did_CH_UpdateMe"
            Dim BlockUpdatesColumnName_forDid_CH_UpdateMetable As String = "CH_Flag"
            CreateTable(PathAndNameOfBlockUpdatesSQLite, BlockUpdates_coords_Tablename, BlockUpdatesColumnName_forcoordstable)
            CreateTable(PathAndNameOfBlockUpdatesSQLite, BlockUpdates_Did_VB_UpdateMe_Tablename, BlockUpdatesColumnName_forDid_VB_UpdateMetable)
            CreateTable(PathAndNameOfBlockUpdatesSQLite, BlockUpdates_Did_CH_UpdateMe_Tablename, BlockUpdatesColumnName_forDid_CH_UpdateMetable)
        End If

    End Sub
    Sub CreateTable(sqliteDB As String, tableName As String, columnName As String)

        Using myConnection As SQLiteConnection = New SQLiteConnection()
            Dim myConnectionString As String
            myConnectionString = "Data Source=" + sqliteDB + ";Version=3; Journal Mode = OFF;"
            myConnection.ConnectionString = myConnectionString
            myConnection.Open()
            Using tx = myConnection.BeginTransaction()
                Using cmd = myConnection.CreateCommand()
                    Try
                        Dim myTableCreate = $"CREATE TABLE {tableName} ({columnName} text primary key) WITHOUT ROWID;"
                        cmd.CommandText = myTableCreate
                        cmd.ExecuteNonQuery()
                    Catch ex As Exception
                        Console.WriteLine(ex.ToString)
                    End Try
                End Using
                tx.Commit()
            End Using
        End Using

    End Sub

#End Region

#Region "Flag"

    Function tryToGetTheCOORDINDEXValue()

        Dim indexTest As String = RetrieveCoord_INDEX_Value()
        Select Case indexTest
            Case "01"
                Return "01"
            Case "02"
                Return "02"
            Case "03"
                Return "03"
            Case "04"
                Return "04"
            Case "05"
                Return "05"
            Case "06"
                Return "06"
            Case "07"
                Return "07"
            Case "08"
                Return "08"
            Case "09"
                Return "09"
            Case "10"
                Return "10"
            Case "aa"
                Return "aa"
            Case "nope"
                Return "bb"
            Case Else
                Return "xx"
        End Select

    End Function

    Sub fillUserCoordinates(playerData As String)

        Dim startAt = playerData.IndexOf("Observor{", 0) + 9
        Dim endAt = playerData.IndexOf(" }Observor", 0) '       
        Dim difference = endAt - startAt
        Dim unsplitObserverCoords As String = playerData.Substring(startAt, difference)
        Dim s As String = New String(unsplitObserverCoords)

        Dim haveFloat As String() = s.Split(New Char() {","c})
        userCoordinates.Item1 = CInt(haveFloat(0)) '    x
        userCoordinates.Item2 = CInt(haveFloat(1)) + 2 'y   +2 for player height. 
        userCoordinates.Item3 = CInt(haveFloat(2)) '    z

    End Sub
#End Region

#Region "InsertSQLiteValues"

    Public allFrameTuples As New List(Of Tuple(Of StringBuilder, StringBuilder))
    Public Record As New Dictionary(Of String, String)
    Function totalFrameTuple()

        Dim black As New StringBuilder
        For Each tupl In allFrameTuples
            If CheckAndUpdateState(tupl.Item2.ToString(), "black") Then
                black.Append(tupl.Item2)                ' Swap these to reverse the colors.
                black.Append(";")
            End If
        Next

        Dim white As New StringBuilder
        For Each tupl In allFrameTuples
            If CheckAndUpdateState(tupl.Item1.ToString(), "white") Then
                white.Append(tupl.Item1)
                white.Append(";")
            End If
        Next

        Dim amalgamationOfFrameTuples = New Tuple(Of StringBuilder, StringBuilder)(black, white)
        Return amalgamationOfFrameTuples

    End Function

    Function CheckAndUpdateState(key As String, color As String) As Boolean
        If Not Record.ContainsKey(key) Then
            ' State 1: Key not in dictionary, add key-value pair and return true
            Record.Add(key, color)
            Return True
        Else
            If Record(key) = color Then
                ' State 2: Key in dictionary and values match, return false
                Return False
            Else
                ' State 3: Key in dictionary and values don't match, update value and return true
                Record(key) = color
                Return True
            End If
        End If
    End Function

    Function testBlocksPlusBlackAndWhite(playerDataNeedsSplitUp As List(Of String), blackAndWhiteTuple As Tuple(Of StringBuilder, StringBuilder))

        Dim flag As String
        flag = playerDataNeedsSplitUp(0)

        Dim WhiteCoordinates As StringBuilder
        WhiteCoordinates = blackAndWhiteTuple.Item2
        Dim BlackCoordinates As StringBuilder
        BlackCoordinates = blackAndWhiteTuple.Item1

        Dim stringForFlashing As StringBuilder = New StringBuilder
        stringForFlashing.Append("BEGIN")
        stringForFlashing.Append(flag)
        stringForFlashing.Append("%")
        stringForFlashing.Append(BlackCoordinates.ToString)
        stringForFlashing.Append("-370,73,-200;-370,72,-200")   'black test

        stringForFlashing.Append("#")
        stringForFlashing.Append(WhiteCoordinates.ToString)
        stringForFlashing.Append("-370,75,-200;-370,74,-200")   'white test
        stringForFlashing.Append(";-64,68,-44")
        stringForFlashing.Append(";-80,70,-29")
        stringForFlashing.Append("END")

        Dim myDesiredOutput As String = stringForFlashing.ToString()
        Dim reducedToSingleLine As String = myDesiredOutput.Replace(vbCr, "").Replace(vbLf, "")
        Return reducedToSingleLine

    End Function
#End Region

#Region "Features"

    Function getlookedatcoords(pdata As List(Of String))

        'proper example: "10Observor{-313.7535383912487,114.11813668075226,-90.27644076604123 }ObservorObserving{ -261.0,213.0,-198.0}Observing"
        Dim flag As String
        flag = pdata(0)
        Dim playerdata As String
        playerdata = pdata(1)
        Dim startAt = playerdata.IndexOf("Observing{ ", 0) + 11
        Dim endAt = playerdata.IndexOf("}Observing", 0)
        Dim difference = endAt - startAt

        Dim unsplitPlayerCoords As String = playerdata.Substring(startAt, difference)
        Return unsplitPlayerCoords

    End Function


#End Region








    Public Class PanelDataManager
        Private CenterCoordinates As (centerX As Integer, centerY As Integer, centerZ As Integer)

        Public North_a As (Integer, Integer, Integer)
        Public North_b As (Integer, Integer, Integer)
        Public North_c As (Integer, Integer, Integer)
        Public North_d As (Integer, Integer, Integer)
        Public South_a As (Integer, Integer, Integer)
        Public South_b As (Integer, Integer, Integer)
        Public South_c As (Integer, Integer, Integer)
        Public South_d As (Integer, Integer, Integer)
        Public West_a As (Integer, Integer, Integer)
        Public West_b As (Integer, Integer, Integer)
        Public West_c As (Integer, Integer, Integer)
        Public West_d As (Integer, Integer, Integer)
        Public East_a As (Integer, Integer, Integer)
        Public East_b As (Integer, Integer, Integer)
        Public East_c As (Integer, Integer, Integer)
        Public East_d As (Integer, Integer, Integer)
        Public Top_a As (Integer, Integer, Integer)
        Public Top_b As (Integer, Integer, Integer)
        Public Top_c As (Integer, Integer, Integer)
        Public Top_d As (Integer, Integer, Integer)
        Public Bottom_a As (Integer, Integer, Integer)
        Public Bottom_b As (Integer, Integer, Integer)
        Public Bottom_c As (Integer, Integer, Integer)
        Public Bottom_d As (Integer, Integer, Integer)

        Public Sub New()
            Console.Write("Enter the X coordinate of the center -575: ")
            Dim centerX As Integer = Convert.ToInt32(Console.ReadLine()) ' -250 ' -575
            Console.Write("Enter the Y coordinate of the center 81: ")
            Dim centerY As Integer = Convert.ToInt32(Console.ReadLine()) ' 73   ' 81
            Console.Write("Enter the Z coordinate of the center -512: ")
            Dim centerZ As Integer = Convert.ToInt32(Console.ReadLine()) ' -78  ' -512

            CenterCoordinates = (centerX, centerY, centerZ)
            populateCorners(CenterCoordinates)
        End Sub

        Sub populateCorners(center As (Integer, Integer, Integer))

            Const halfSideLength As Integer = 121
            Dim A As (Integer, Integer, Integer) = (center.Item1 - halfSideLength, center.Item2, center.Item3 - halfSideLength) ' Calculate the coordinates
            Dim B As (Integer, Integer, Integer) = (center.Item1 + halfSideLength, center.Item2, center.Item3 - halfSideLength) ' of the four points,
            Dim C As (Integer, Integer, Integer) = (center.Item1 + halfSideLength, center.Item2, center.Item3 + halfSideLength) ' without using fractions.
            Dim D As (Integer, Integer, Integer) = (center.Item1 - halfSideLength, center.Item2, center.Item3 + halfSideLength)
            Dim bottom As New Dictionary(Of String, (Integer, Integer, Integer))
            bottom.Add("a", A)
            bottom.Add("b", B)
            bottom.Add("c", C)
            bottom.Add("d", D)
            Const elevation As Integer = 162
            Dim Top As New Dictionary(Of String, (Integer, Integer, Integer))
            Top.Add("c", (bottom("b").Item1, bottom("b").Item2 + elevation, bottom("b").Item3)) ' Top
            Top.Add("d", (bottom("a").Item1, bottom("a").Item2 + elevation, bottom("a").Item3))
            Top.Add("b", (bottom("c").Item1, bottom("c").Item2 + elevation, bottom("c").Item3))
            Top.Add("a", (bottom("d").Item1, bottom("d").Item2 + elevation, bottom("d").Item3))
            Dim North As New Dictionary(Of String, (Integer, Integer, Integer)) ' North
            North.Add("c", bottom("b"))
            North.Add("d", bottom("a"))
            North.Add("b", (North("c").Item1, North("c").Item2 + elevation, North("c").Item3))
            North.Add("a", (North("d").Item1, North("d").Item2 + elevation, North("d").Item3))
            Dim South As New Dictionary(Of String, (Integer, Integer, Integer)) ' South
            South.Add("c", bottom("d"))
            South.Add("d", bottom("c"))
            South.Add("b", (South("c").Item1, South("c").Item2 + elevation, South("c").Item3))
            South.Add("a", (South("d").Item1, South("d").Item2 + elevation, South("d").Item3))
            Dim West As New Dictionary(Of String, (Integer, Integer, Integer)) ' West
            West.Add("c", bottom("a"))
            West.Add("d", bottom("d"))
            West.Add("b", (West("c").Item1, West("c").Item2 + elevation, West("c").Item3))
            West.Add("a", (West("d").Item1, West("d").Item2 + elevation, West("d").Item3))
            Dim East As New Dictionary(Of String, (Integer, Integer, Integer)) ' East
            East.Add("c", bottom("c"))
            East.Add("d", bottom("b"))
            East.Add("b", (East("c").Item1, East("c").Item2 + elevation, East("c").Item3))
            East.Add("a", (East("d").Item1, East("d").Item2 + elevation, East("d").Item3))
            ' only then 
            Top("a") = (Top("a").Item1 + 1, Top("a").Item2, Top("a").Item3 - 1) ' Top
            Top("b") = (Top("b").Item1 - 1, Top("b").Item2, Top("b").Item3 - 1) ' Axis adjustments
            Top("c") = (Top("c").Item1 - 1, Top("c").Item2, Top("c").Item3 + 1)
            Top("d") = (Top("d").Item1 + 1, Top("d").Item2, Top("d").Item3 + 1)
            bottom("a") = (bottom("a").Item1 + 1, bottom("a").Item2, bottom("a").Item3 + 1) ' bottom
            bottom("b") = (bottom("b").Item1 - 1, bottom("b").Item2, bottom("b").Item3 + 1)
            bottom("c") = (bottom("c").Item1 - 1, bottom("c").Item2, bottom("c").Item3 - 1)
            bottom("d") = (bottom("d").Item1 + 1, bottom("d").Item2, bottom("d").Item3 - 1)
            West("a") = (West("a").Item1, West("a").Item2 - 1, West("a").Item3 - 1) ' West
            West("b") = (West("b").Item1, West("b").Item2 - 1, West("b").Item3 + 1)
            West("c") = (West("c").Item1, West("c").Item2 + 1, West("c").Item3 + 1)
            West("d") = (West("d").Item1, West("d").Item2 + 1, West("d").Item3 - 1)
            North("a") = (North("a").Item1 + 1, North("a").Item2 - 1, North("a").Item3) ' North
            North("b") = (North("b").Item1 - 1, North("b").Item2 - 1, North("b").Item3)
            North("c") = (North("c").Item1 - 1, North("c").Item2 + 1, North("c").Item3)
            North("d") = (North("d").Item1 + 1, North("d").Item2 + 1, North("d").Item3)
            South("a") = (South("a").Item1 - 1, South("a").Item2 - 1, South("a").Item3) ' South
            South("b") = (South("b").Item1 + 1, South("b").Item2 - 1, South("b").Item3)
            South("c") = (South("c").Item1 + 1, South("c").Item2 + 1, South("c").Item3)
            South("d") = (South("d").Item1 - 1, South("d").Item2 + 1, South("d").Item3)
            East("a") = (East("a").Item1, East("a").Item2 - 1, East("a").Item3 + 1) ' East
            East("b") = (East("b").Item1, East("b").Item2 - 1, East("b").Item3 - 1)
            East("c") = (East("c").Item1, East("c").Item2 + 1, East("c").Item3 - 1)
            East("d") = (East("d").Item1, East("d").Item2 + 1, East("d").Item3 + 1)



            Bottom_a = bottom("a")
            Bottom_b = bottom("b")
            Bottom_c = bottom("c")
            Bottom_d = bottom("d")
            Top_a = Top("a")
            Top_b = Top("b")
            Top_c = Top("c")
            Top_d = Top("d")
            North_a = North("a")
            North_b = North("b")
            North_c = North("c")
            North_d = North("d")
            South_a = South("a")
            South_b = South("b")
            South_c = South("c")
            South_d = South("d")
            West_a = West("a")
            West_b = West("b")
            West_c = West("c")
            West_d = West("d")
            East_a = East("a")
            East_b = East("b")
            East_c = East("c")
            East_d = East("d")

            panelsArray = New(PanelType, (Integer, Integer, Integer), (Integer, Integer, Integer))() {
            (PanelType.BottomPanel, Bottom_a, Bottom_c), ' for detecting the coordinates which are the ones the vector passes through  
            (PanelType.NorthPanel, North_d, North_b), ' from the point in question ->thru the viewer.
            (PanelType.EastPanel, East_d, East_b),
            (PanelType.SouthPanel, South_d, South_b),
            (PanelType.WestPanel, West_d, West_b),
            (PanelType.TopPanel, Top_d, Top_b)}

            panelNormalsArray = New(PanelType, (Integer, Integer, Integer), (Integer, Integer, Integer))() {
            (PanelType.BottomPanel, CalculateNormal(Bottom_a, Bottom_b, Bottom_c), Bottom_c), ' for detecting the orientation of the plane
            (PanelType.NorthPanel, CalculateNormal(North_a, North_b, North_c), North_c), ' in panelBounds.
            (PanelType.EastPanel, CalculateNormal(East_a, East_b, East_c), East_c),
            (PanelType.SouthPanel, CalculateNormal(South_a, South_b, South_c), South_c),
            (PanelType.WestPanel, CalculateNormal(West_a, West_b, West_c), West_c),
            (PanelType.TopPanel, CalculateNormal(Top_a, Top_b, Top_c), Top_c)}

            panelName_CornerArray = New(String, (Integer, Integer, Integer))() {
            ("Bottom_a", Bottom_a), ' =* For Universal Reference *=
            ("Bottom_b", Bottom_b),
            ("Bottom_c", Bottom_c),
            ("Bottom_d", Bottom_d),
            ("North_a", North_a),
            ("North_b", North_b),
            ("North_c", North_c),
            ("North_d", North_d),
            ("East_a", East_a),
            ("East_b", East_b),
            ("East_c", East_c),
            ("East_d", East_d),
            ("South_a", South_a),
            ("South_b", South_b),
            ("South_c", South_c),
            ("South_d", South_d),
            ("West_a", West_a),
            ("West_b", West_b),
            ("West_c", West_c),
            ("West_d", West_d),
            ("Top_a", Top_a),
            ("Top_b", Top_b),
            ("Top_c", Top_c),
            ("Top_d", Top_d)}

        End Sub
        Function CalculateNormal(firstCorner As (Integer, Integer, Integer), secondCorner As (Integer, Integer, Integer), thirdCorner As (Integer, Integer, Integer)) As (Integer, Integer, Integer)
            Dim edgeVectorFirstToSecond = (secondCorner.Item1 - firstCorner.Item1, secondCorner.Item2 - firstCorner.Item2, secondCorner.Item3 - firstCorner.Item3)
            Dim edgeVectorFirstToThird = (thirdCorner.Item1 - firstCorner.Item1, thirdCorner.Item2 - firstCorner.Item2, thirdCorner.Item3 - firstCorner.Item3)
            Dim crossProduct = (edgeVectorFirstToSecond.Item2 * edgeVectorFirstToThird.Item3 - edgeVectorFirstToSecond.Item3 * edgeVectorFirstToThird.Item2,
                             edgeVectorFirstToSecond.Item3 * edgeVectorFirstToThird.Item1 - edgeVectorFirstToSecond.Item1 * edgeVectorFirstToThird.Item3,
                             edgeVectorFirstToSecond.Item1 * edgeVectorFirstToThird.Item2 - edgeVectorFirstToSecond.Item2 * edgeVectorFirstToThird.Item1)
            Return crossProduct
        End Function


    End Class








    Public Enum PanelType
        BottomPanel
        NorthPanel
        EastPanel
        SouthPanel
        WestPanel
        TopPanel
    End Enum
End Module
