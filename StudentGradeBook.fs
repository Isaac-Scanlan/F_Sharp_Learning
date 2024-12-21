module StudentGradeBook


type Student = 
    {
        Name: string;
        Grades: float[];
        AverageGrade: float
        StudentInfo: string
    }

let average (grades: float[]) : float option=
    match grades.Length with
    | 0 -> None
    | _ -> Some (grades |> Array.average)

let studentInfo name grades average =
    let fullGrades = grades |> Array.map string |> String.concat ", "
    $"Name: {name}, Grades: {fullGrades}, GPA: {average}"

let printStudentInfo students =
    students
    |> Seq.iter (fun student -> printfn $"{student.StudentInfo}")

let createStudent name grades =
    let averageGrade = average grades
    let avg = averageGrade |> Option.defaultValue 0.0
    {
        Name = name;
        Grades = grades;
        AverageGrade = avg
        StudentInfo = studentInfo name grades avg
    }

let RunStudentTest () =
    let students = ResizeArray<Student>()
    students.Add(createStudent "John" [|78.0; 89.3; 22.1|])
    students.Add(createStudent "Peter" [|82.3; 81.6; 48.7|])
    students.Add(createStudent "Pat" [|93.1; 95.1; 52.1|])
    students.Add(createStudent "Susan" [|79.4; 83.2; 50.3|])

    printStudentInfo students

    let averageGrades = students |> Seq.map (fun student -> student.AverageGrade)

    let isPassing student = student.AverageGrade > 74.0
    let studentName student = student.Name

    let passingStudents = 
        students 
        |> Seq.filter isPassing
        |> Seq.map studentName 

    passingStudents
    |> Seq.iter (fun student -> printfn $"{student} passed the year")

    printfn "The rest failed"
    0